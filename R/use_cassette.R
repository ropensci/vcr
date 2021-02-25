#' Use a cassette to record HTTP requests
#'
#' @export
#' @inherit check_cassette_names details
#' @param name The name of the cassette. vcr will check this to ensure it
#' is a valid file name. Not allowed: spaces, file extensions, control
#' characters (e.g., `\n`), illegal characters ('/', '?', '<', '>', '\\', ':',
#' '*', '|', and '\"'), dots alone (e.g., '.', '..'), Windows reserved
#' words (e.g., 'com1'), trailing dots (can cause problems on Windows),
#' names longer than 255 characters. See section "Cassette names"
#' @param ... a block of code containing one or more requests (required). Use
#' curly braces to encapsulate multi-line code blocks. If you can't pass a code
#' block use [insert_cassette()] instead.
#' @param record The record mode (default: `"once"`). See [recording] for a
#' complete list of the different recording modes.
#' @param match_requests_on List of request matchers
#' to use to determine what recorded HTTP interaction to replay. Defaults to
#' `["method", "uri"]`. The built-in matchers are "method", "uri", "host",
#' "path", "headers", "body" and "query"
#' @param update_content_length_header (logical) Whether or
#' not to overwrite the `Content-Length` header of the responses to
#' match the length of the response body. Default: `FALSE`
#' @param allow_playback_repeats (logical) Whether or not to
#' allow a single HTTP interaction to be played back multiple times.
#' Default: `FALSE`.
#' @param serialize_with (character) Which serializer to use.
#' Valid values are "yaml" (default) and "json". Note that you can have
#' multiple cassettes with the same name as long as they use different
#' serializers; so if you only want one cassette for a given cassette name,
#' make sure to not switch serializers, or clean up files you no longer need.
#' @param persist_with (character) Which cassette persister to
#' use. Default: "file_system". You can also register and use a
#' custom persister.
#' @param preserve_exact_body_bytes (logical) Whether or not
#' to base64 encode the bytes of the requests and responses for
#' this cassette when serializing it. See also `preserve_exact_body_bytes`
#' in [vcr_configure()]. Default: `FALSE`
#' @param re_record_interval (integer) How frequently (in seconds) the
#' cassette should be re-recorded. default: `NULL` (not re-recorded)
#' @param clean_outdated_http_interactions (logical) Should outdated
#' interactions be recorded back to file? default: `FALSE`
#' @param record_separate_redirects (logical) If http redirects are
#' encountered in http requests, should we record the intermediate requests?
#' If `FALSE`, only the final response with a non-3xx series status code
#' is recorded. default: `FALSE`
#'
#' @details A run down of the family of top level \pkg{vcr} functions
#'
#' - `use_cassette` Initializes a cassette. Returns the inserted
#'  cassette.
#' - `insert_cassette` Internally used within `use_cassette`
#' - `eject_cassette` ejects the current cassette. The cassette
#'  will no longer be used. In addition, any newly recorded HTTP interactions
#'  will be written to disk.
#'
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
#' @seealso [insert_cassette()], [eject_cassette()]
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir())
#'
#' use_cassette(name = "apple7", {
#'   cli <- HttpClient$new(url = "https://httpbin.org")
#'   resp <- cli$get("get")
#' })
#' readLines(file.path(tempdir(), "apple7.yml"))
#'
#' # preserve exact body bytes - records in base64 encoding
#' use_cassette("things4", {
#'   cli <- crul::HttpClient$new(url = "https://httpbin.org")
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
#'   res <- GET("https://httpbin.org/get")
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

use_cassette <- function(name, ...,
  record = NULL,
  match_requests_on = NULL,
  update_content_length_header = FALSE,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  persist_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  record_separate_redirects = FALSE) {

  cassette <- insert_cassette(name,
    record = record,
    match_requests_on = match_requests_on,
    update_content_length_header = update_content_length_header,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with,
    persist_with = persist_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions,
    record_separate_redirects = record_separate_redirects
  )
  if (is.null(cassette)) {
    force(...)
    return(NULL)
  }
  on.exit(cassette$eject())
  cassette$call_block(...)
  if (record_separate_redirects) {
    while (redirects_remaining(cassette)) {
      rel_path <- last(cassette$merged_interactions())[[1]]$response$headers$location
      cassette$request_original <-
        update_relative(cassette$request_original, rel_path)
      switch(http_client(cassette$request_original),
        crul = RequestHandlerCrul$new(cassette$request_original)$handle(),
        httr = RequestHandlerHttr$new(cassette$request_original)$handle()
      )
    }
    # what we need to do:
    # 1. before any real requests, set followlocation=0L, then
    # 2. one request at a time, capturing the link to follow each time, and
    # 3. end when there are no more links to follow
    # FIXME: WHY IS THE RESPONSE FROM THE FIRST REQUEST? NEED TO GIVE BACK THE RESPONSE FROM THE LAST REQUEST
  }
  return(cassette)
}


scode <- function(x) {
  if ("status_code" %in% names(x)) return(x$status_code)
  return(x$status$status_code)
}
last_http_status <- function(cas) {
  z <- last(cas$merged_interactions())
  if (!length(z)) "" else scode(z[[1]]$response)
  # $status$status_code
}
redirects_remaining <- function(cas) {
  stat <- as.character(last_http_status(cas))
  sac$last_http_status_code <- stat
  if (!nzchar(stat)) return(TRUE)
  stat %in% c("301", "302", "303", "307", "308")
}
http_client <- function(x) {
  ifelse(is.list(x$url), "crul", "httr")
}
update_relative <- function(req, path) {
  pkg <- http_client(req)
  tmp <- urltools::url_parse(switch(pkg, crul=req$url$url, httr=req$url))
  tmp$path <- sub("^/", "", path)
  if (pkg == "crul") {
    req$url$url <- urltools::url_compose(tmp)
  } else {
    req$url <- urltools::url_compose(tmp)
  }
  if (pkg == "crul")
    curl::handle_setopt(req$url$handle, followlocation = 0L)
  return(req)
}
