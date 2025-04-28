vcr_request <- function(method, uri, body = NULL, headers = list()) {
  if (is.list(body)) {
    body <- paste(names(body), body, sep = "=", collapse = ",")
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
