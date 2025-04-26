vcr_response <- function(status, headers = list(), body = NULL, disk = NULL) {
  if (is.list(body)) {
    body <- paste(names(body), body, sep = "=", collapse = ",")
  }

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
