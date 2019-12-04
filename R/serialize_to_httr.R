# generate actual httr response
serialize_to_httr <- function(request, response) {
  # request
  req <- webmockr::RequestSignature$new(
    method = request$method,
    uri = request$uri,
    options = list(
      body = request$body %||% NULL,
      headers = request$headers %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = response$disk
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  response_body <- if (response$disk) {
    structure(response$body, class = "path")
  } else {
    response$body
  }
  resp$set_body(response_body, response$disk %||% FALSE)
  # resp$set_body(if ("string" %in% names(bod)) bod$string else bod)
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  # resp$set_status(status = response$status %||% 200)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate httr response
  webmockr::build_httr_response(req, resp)
}
