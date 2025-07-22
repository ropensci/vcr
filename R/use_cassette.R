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
#' @param dir The directory where the cassette will be stored. If unspecified,
#'   (and hasn't been set in [vcr_configure()]) will use `test_path("_vcr")`.
#' @param record Record mode that dictates how HTTP requests/responses are
#'   recorded. Possible values are:
#'
#'   * **once**, the default: Replays recorded interactions, records new ones
#'     if no cassette exists, and errors on new requests if cassette exists.
#'   * **none**: Replays recorded interactions, and errors on any new requests.
#'     Guarantees that no HTTP requests occur.
#'   * **new_episodes**: Replays recorded interactions and always records new
#'     ones, even if similar interactions exist.
#'   * **all**: Never replays recorded interactions, always recording new.
#'     Useful for re-recording outdated responses or logging all HTTP requests.
#' @param match_requests_on Character vector of request matchers used to
#'   determine which recorded HTTP interaction to replay. The default matches
#'   on the `"method"`, `"uri"`, and either `"body"` (if present)
#'   or `"body_json"` (if the content-type is `application/json`).
#'
#'   The full set of possible values are:
#'
#'   * `method`: the HTTP method.
#'   * `uri`: the complete request URI, excluding the port.
#'   * `uri_with_port`: the complete request URI, including the port.
#'   * `host`: the **host** component of the URI.
#'   * `path`: the **path** component of the URI.
#'   * `query`: the **query** component of the URI.
#'   * `body`: the request body.
#'   * `body_json`: the request body, parsed as JSON.
#'   * `header`: all request headers.
#'
#'   If more than one is specified, all components must match in order for the
#'   request to match. If not supplied, defaults to `c("method", "uri")`.
#'
#'   Note that the request header and body will only be included in the
#'   cassette if  `match_requests_on` includes "header" or "body" respectively.
#'   This keeps the recorded request as lightweight as possible.
#'
#' @param serialize_with (string) Which serializer to use:
#'   `"yaml"` (the default), `"json"`, or `"qs2"`.
#' @param preserve_exact_body_bytes (logical) Force a binary (base64)
#'   representation of the request and response bodies? By default, vcr
#'   will look at the `Content-Type` header to determine if this is necessary,
#'   but if it doesn't work you can set `preserve_exact_body_bytes = TRUE` to
#'   force it.
#' @param re_record_interval (integer) How frequently (in seconds) the
#' cassette should be re-recorded. Default: `NULL` (not re-recorded).
#' @param warn_on_empty (logical) Warn if the cassette is ejected but no interactions
#'   have been recorded. Default: `NULL` (inherits from global configuration).
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
#' ## then use record mode 'none' to see its behavior
#' two <- use_cassette("none_eg", (res2 <- conn$get("get")), record = "none")
#' two; res2
#' }

use_cassette <- function(
  name,
  ...,
  dir = NULL,
  record = NULL,
  match_requests_on = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  warn_on_empty = NULL
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
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    warn_on_empty = warn_on_empty
  )

  # force all arguments
  withCallingHandlers(
    list(...),
    error = function(cnd) {
      # Don't warn if there was also an error
      cassette$warn_on_empty <- FALSE
    }
  )

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
  dir = NULL,
  record = NULL,
  match_requests_on = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  warn_on_empty = NULL,
  frame = parent.frame()
) {
  check_string(name, allow_empty = FALSE)
  check_cassette_name(name)
  check_string(dir, allow_null = TRUE)
  check_request_matchers(match_requests_on)
  check_record_mode(record)
  check_bool(preserve_exact_body_bytes, allow_null = TRUE)
  check_number_whole(re_record_interval, allow_null = TRUE)
  check_bool(warn_on_empty, allow_null = TRUE)

  cassette <- insert_cassette(
    name,
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    warn_on_empty = warn_on_empty
  )
  if (!is.null(cassette)) {
    defer(eject_cassette(), frame)
  }

  invisible(cassette)
}

check_cassette_name <- function(x, call = caller_env()) {
  if (any(x %in% cassette_names())) {
    cli::cli_abort(
      "{.arg name}, {.str {x}}, must not be the same as an existing cassette.",
      call = call
    )
  }

  if (grepl("\\s", x)) {
    cli::cli_abort(
      "{.arg name}, {.str {x}}, must not contain spaces.",
      call = call
    )
  }

  if (grepl("\\.yml$|\\.yaml$", x)) {
    cli::cli_abort(
      "{.arg name}, {.str {x}}, must not include an extension.",
      call = call
    )
  }

  # the below adapted from fs::path_sanitize, which adapted
  # from the npm package sanitize-filename
  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"
  if (grepl(illegal, x)) {
    cli::cli_abort(
      "{.arg name} must not contain '/', '?', '<', '>', '\\', ':', '*', '|', or '\"'",
      call = call
    )
  }
  if (grepl(control, x)) {
    cli::cli_abort(
      "{.arg name} must not contain control characters.",
      call = call
    )
  }
  if (grepl(reserved, x)) {
    cli::cli_abort(
      "{.arg name} must not be '.', '..', etc.",
      call = call
    )
  }
  if (grepl(windows_reserved, x)) {
    cli::cli_abort(
      "{.arg name} must not contain reserved windows strings.",
      call = call
    )
  }
  if (grepl(windows_trailing, x)) {
    cli::cli_abort("{.arg name} must not end in '.'.", call = call)
  }
  if (nchar(x) > 255) {
    cli::cli_abort("{.arg name} must be less than 256 characters.", call = call)
  }

  invisible()
}


#' Determine if vcr is recording/replaying
#'
#' [local_cassette()] and `use_cassette()` set the `VCR_IS_RECORDING`
#' and `VCR_IS_REPLAYING` environment variables to make it easy to determine
#' vcr state without taking a dependency on vcr. These functions show you
#' how to use them; we expect you to copy and paste these functions into your
#' own package
#'
#' @export
#' @return `TRUE` or `FALSE`.
is_recording <- function() {
  as.logical(Sys.getenv("VCR_IS_RECORDING", "FALSE"))
}

#' @export
#' @rdname is_recording
is_replaying <- function() {
  as.logical(Sys.getenv("VCR_IS_REPLAYING", "FALSE"))
}
