disk_true <- function(x) {
  if (is.null(x)) return(FALSE)
  assert(x, "logical")
  return(x)
}

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
      disk = response$disk,
      fields = request$fields %||% NULL,
      output = request$output %||% NULL
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  # in vcr >= v0.4, "disk" is in the response, but in older versions
  # its missing - use response$body if disk is not present
  response_body <- response$body
  if ("disk" %in% names(response) && disk_true(response$disk)) {
    response_body <- if (response$disk) {
      structure(response$body, class = "path")
    } else {
      response$body
    }
  }
  resp$set_body(response_body, response$disk %||% FALSE)
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate httr response
  webmockr::build_httr_response(as_httr_request(req), resp)
}

keep_last <- function(...) {
  x <- c(...)
  x[!duplicated(names(x), fromLast = TRUE)]
}
httr_ops <- function(method, w) {
  # if (!length(w)) return(w)
  if (tolower(method) == "get") w$httpget <- TRUE
  if (tolower(method) == "post") w$post <- TRUE
  if (!tolower(method) %in% c("get", "post")) w$customrequest <- toupper(method)
  return(w)
}
as_httr_request <- function(x) {
  structure(list(method = toupper(x$method), url = x$url$url,
    headers = keep_last(x$headers), fields = x$fields,
    options = httr_ops(x$method, compact(keep_last(x$options))),
    auth_token = x$auth, output = x$output), class = "request")
}
