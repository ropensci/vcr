#' Use a cassette to record HTTP requests
#'
#' @description
#' `use_cassette(...)` uses a cassette for the code in `...`;
#' `local_cassette()` uses a cassette for the current function scope (e.g.
#' for one test). Learn more in `vignette("vcr")`.
#'
#' Note that defaults for most arguments are controlled by [vcr_configure()],
#' so you may want to use that instead if you are changing the defaults for
#' all cassettes.
#'
#' @export
#' @param name The name of the cassette. This is used to name a file on
#'   disk, so it must be valid file name.
#' @param ... a block of code containing one or more requests (required). Use
#' curly braces to encapsulate multi-line code blocks. If you can't pass a code
#' block use [insert_cassette()] instead.
#' @param dir The directory where the cassette will be stored. Defaults to
#'   `test_path("_vcr")`.
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
