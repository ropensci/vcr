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

vcr_response <- function(status, headers = list(), body = NULL, disk = FALSE) {
  check_number_whole(status)
  if (!is.list(headers)) {
    stop_input_type(headers, "a list")
  }
  if (!is_string(body) && !is.raw(body) && !is.null(body)) {
    stop_input_type(body, "a string, raw vector, or NULL")
  }
  check_bool(disk)

  structure(
    list(
      status = status,
      headers = headers,
      body = body,
      disk = disk
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
