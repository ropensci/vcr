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
