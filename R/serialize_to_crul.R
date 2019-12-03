# generate actual crul response
serialize_to_crul <- function(request, response) {
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
  bod <- response$body
  resp$set_body(if ("string" %in% names(bod)) bod$string else bod,
    response$disk %||% FALSE)
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  # resp$set_status(status = response$status %||% 200)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate crul response
  webmockr::build_crul_response(req, resp)
}
