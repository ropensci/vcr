# generate actual httr2 response
# request <- httr2::request("https://hb.opencpu.org/301")
# 
serialize_to_httr2 <- function(request, response) {
  # request
  req <- webmockr::RequestSignature$new(
    method = request$method,
    uri = request$uri,
    options = list(
      body = request$body %||% NULL,
      headers = request$headers %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = if (inherits(response$body, "httr2_path")) response$body %||% NULL,
      fields = request$fields %||% NULL,
      output = request$output %||% NULL
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  resp$set_body(response$body, inherits(response$body, "httr2_path"))
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate httr2 response
  webmockr::build_httr2_response(as_httr2_request(req), resp)
}

as_httr2_request <- function(x) {
  structure(list(url = x$url$url, method = toupper(x$method),
    headers = x$headers, body = x$body, fields = x$fields,
    options = x$options, policies = x$policies),
  class = "httr2_request")
}
