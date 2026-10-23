vcr_interaction <- function(
  request,
  response,
  recorded_at = Sys.time(),
  error_call = caller_env()
) {
  check_vcr_request(request, error_call = error_call)
  check_vcr_response(response, error_call = error_call)

  if (!inherits(recorded_at, "POSIXct") || length(recorded_at) != 1) {
    stop_input_type(recorded_at, "a date-time", error_call = error_call)
  }

  structure(
    list(
      request = request,
      response = response,
      recorded_at = recorded_at
    ),
    class = "vcr_interaction"
  )
}

vcr_request <- function(method, uri, body = NULL, headers = list()) {
  check_string(method)
  check_string(uri)
  if (!is_string(body) && !is.raw(body) && !is.list(body) && !is.null(body)) {
    stop_input_type(body, "a string, raw vector, list, or NULL")
  }
  if (!is.list(headers)) {
    stop_input_type(headers, "a list")
  }

  structure(
    list(
      method = toupper(method),
      uri = uri,
      body = body,
      headers = headers
    ),
    class = "vcr_request"
  )
}

#' @noRd
#' @description `body_chunks` is only set for streamed responses. It records
#' the size in bytes of each chunk the body was received in, so that replay
#' can hand them back with the same boundaries. `body` always holds the
#' complete body.
vcr_response <- function(
  status,
  headers = list(),
  body = NULL,
  disk = FALSE,
  body_chunks = NULL
) {
  check_number_whole(status)
  if (!is.list(headers)) {
    stop_input_type(headers, "a list")
  }
  if (!is_string(body) && !is.raw(body) && !is.null(body)) {
    stop_input_type(body, "a string, raw vector, or NULL")
  }
  check_bool(disk)
  if (!is.null(body_chunks)) {
    if (
      !is.numeric(body_chunks) || anyNA(body_chunks) || any(body_chunks < 0)
    ) {
      stop_input_type(
        body_chunks,
        "a vector of non-negative whole numbers, or NULL"
      )
    }
    body_chunks <- as.integer(body_chunks)
  }

  structure(
    list(
      status = status,
      headers = headers,
      body = body,
      disk = disk,
      body_chunks = body_chunks
    ),
    class = "vcr_response"
  )
}

# Helpers ---------------------------------------------------------------------

check_vcr_request <- function(request, error_call = caller_env()) {
  if (!inherits(request, "vcr_request")) {
    stop_input_type(request, "a <vcr_request>", error_call = error_call)
  }
}

check_vcr_response <- function(response, error_call = caller_env()) {
  if (!inherits(response, "vcr_response")) {
    stop_input_type(response, "a <vcr_response>", error_call = error_call)
  }
}
