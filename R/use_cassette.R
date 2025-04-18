#' Use a cassette to record HTTP requests
#'
#' `use_cassette(...)` uses a cassette for the code in `...`,
#' `local_cassette()` uses a cassette for the current function scope (e.g.
#' for one test).
#'
#' @export
#' @param name The name of the cassette. This is used to name a file on
#'   disk, so it must be valid file name.
#' @param ... a block of code containing one or more requests (required). Use
#' curly braces to encapsulate multi-line code blocks. If you can't pass a code
#' block use [insert_cassette()] instead.
#' @param record The record mode (default: `"once"`). See [recording] for a
#' complete list of the different recording modes.
#' @param match_requests_on List of request matchers
#' to use to determine what recorded HTTP interaction to replay. Defaults to
#' `["method", "uri"]`. The built-in matchers are "method", "uri", "host",
#' "path", "headers", "body" and "query"
#' @param allow_playback_repeats (logical) Whether or not to
#' allow a single HTTP interaction to be played back multiple times.
#' Default: `FALSE`.
#' @param serialize_with (character) Which serializer to use.
#' Valid values are "yaml" (default) and "json". Note that you can have
#' multiple cassettes with the same name as long as they use different
#' serializers; so if you only want one cassette for a given cassette name,
#' make sure to not switch serializers, or clean up files you no longer need.
#' @param preserve_exact_body_bytes (logical) Whether or not
#' to base64 encode the bytes of the requests and responses for
#' this cassette when serializing it. See also `preserve_exact_body_bytes`
#' in [vcr_configure()]. Default: `FALSE`
#' @param re_record_interval (integer) How frequently (in seconds) the
#' cassette should be re-recorded. default: `NULL` (not re-recorded)
#' @param clean_outdated_http_interactions (logical) Should outdated
#' interactions be recorded back to file? default: `FALSE`
#' @seealso [insert_cassette()] and [eject_cassette()] for the underlying
#'   functions.
#' @section Cassette options:
#'
#' Default values for arguments controlling cassette behavior are
#' inherited from vcr's global configuration. See [`vcr_configure()`] for a
#' complete list of options and their default settings. You can override these
#' options for a specific cassette by changing an argument's value to something
#' other than `NULL` when calling either `insert_cassette()` or
#' `use_cassette()`.
#'
#' @section Behavior:
#' This function handles a few different scenarios:
#'
#' - when everything runs smoothly, and we return a `Cassette` class object
#' so you can inspect the cassette, and the cassette is ejected
#' - when there is an invalid parameter input on cassette creation,
#' we fail with a useful message, we don't return a cassette, and the
#' cassette is ejected
#' - when there is an error in calling your passed in code block,
#' we return with a useful message, and since we use `on.exit()`
#' the cassette is still ejected even though there was an error,
#' but you don't get an object back
#' - whenever an empty cassette (a yml/json file) is found, we delete it
#' before returning from the `use_cassette()` function call. we achieve
#' this via use of `on.exit()` so an empty cassette is deleted even
#' if there was an error in the code block you passed in
#'
#' @section Cassettes on disk:
#' Note that _"eject"_ only means that the R session cassette is no longer
#' in use. If any interactions were recorded to disk, then there is a file
#' on disk with those interactions.
#'
#' @section Using with tests (specifically \pkg{testthat}):
#' There's a few ways to get correct line numbers for failed tests and
#' one way to not get correct line numbers:
#'
#' *Correct*: Either wrap your `test_that()` block inside your `use_cassette()`
#' block, OR if you put your `use_cassette()` block inside your `test_that()`
#' block put your `testthat` expectations outside of the `use_cassette()`
#' block.
#'
#' *Incorrect*: By wrapping the `use_cassette()` block inside your
#' `test_that()` block with your \pkg{testthat} expectations inside the
#' `use_cassette()` block, you'll only get the line number that the
#' `use_cassette()` block starts on.
#'
#' @return an object of class `Cassette`
#'
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir())
#'
#' use_cassette(name = "apple7", {
#'   cli <- HttpClient$new(url = "https://hb.opencpu.org")
#'   resp <- cli$get("get")
#' })
#' readLines(file.path(tempdir(), "apple7.yml"))
#'
#' # preserve exact body bytes - records in base64 encoding
#' use_cassette("things4", {
#'   cli <- crul::HttpClient$new(url = "https://hb.opencpu.org")
#'   bbb <- cli$get("get")
#' }, preserve_exact_body_bytes = TRUE)
#' ## see the body string value in the output here
#' readLines(file.path(tempdir(), "things4.yml"))
#'
#' # cleanup
#' unlink(file.path(tempdir(), c("things4.yml", "apple7.yml")))
#'
#'
#' # with httr
#' library(vcr)
#' library(httr)
#' vcr_configure(dir = tempdir(), log = TRUE, log_opts = list(file = file.path(tempdir(), "vcr.log")))
#'
#' use_cassette(name = "stuff350", {
#'   res <- GET("https://hb.opencpu.org/get")
#' })
#' readLines(file.path(tempdir(), "stuff350.yml"))
#'
#' use_cassette(name = "catfact456", {
#'   res <- GET("https://catfact.ninja/fact")
#' })
#'
#' # record mode: none
#' library(crul)
#' vcr_configure(dir = tempdir())
#'
#' ## make a connection first
#' conn <- crul::HttpClient$new("https://eu.httpbin.org")
#' ## this errors because 'none' disallows any new requests
#' # use_cassette("none_eg", (res2 <- conn$get("get")), record = "none")
#' ## first use record mode 'once' to record to a cassette
#' one <- use_cassette("none_eg", (res <- conn$get("get")), record = "once")
#' one; res
#' ## then use record mode 'none' to see it's behavior
#' two <- use_cassette("none_eg", (res2 <- conn$get("get")), record = "none")
#' two; res2
#' }

use_cassette <- function(
  name,
  ...,
  record = NULL,
  match_requests_on = NULL,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL
) {
  check_required(name)
  if (missing(...)) {
    cli::cli_abort(c(
      "{.arg ...} must not be empty.",
      i = "Do you want {.fn local_cassette} instead?"
    ))
  }

  cassette <- local_cassette(
    name,
    record = record,
    match_requests_on = match_requests_on,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions
  )

  # force all arguments
  list(...)

  invisible(cassette)
}

#' @rdname use_cassette
#' @export
#' @param frame Attach exit handlers to this environment. Typically, this
#'   should be either the current environment or a parent frame (accessed
#'   through [parent.frame()]). See `vignette("withr", package = "withr")`
#'   for more details.
local_cassette <- function(
  name,
  record = NULL,
  match_requests_on = NULL,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  frame = parent.frame()
) {
  cassette <- insert_cassette(
    name,
    record = record,
    match_requests_on = match_requests_on,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions
  )
  if (!is.null(cassette)) {
    withr::defer(eject_cassette(), envir = frame)
  }

  invisible(cassette)
}
