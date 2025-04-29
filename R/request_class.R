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
