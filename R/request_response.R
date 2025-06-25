request_summary <- function(request, request_matchers = NULL) {
  check_vcr_request(request)
  check_request_matchers(request_matchers)

  atts <- c(request$method, request$uri)
  if ("body" %in% request_matchers) {
    atts <- c(atts, substring(request$body, 0, 80))
  }
  if ("headers" %in% request_matchers) {
    atts <- c(atts, request$headers)
  }
  paste0(atts, collapse = " ")
}

response_summary <- function(response) {
  check_vcr_response(response)

  # if body is raw, state that it's raw
  if (is.null(response$body)) {
    body <- ""
  } else if (is.raw(response$body)) {
    body <- sprintf("%d bytes of binary data", length(response$body))
  } else {
    idx <- match("content-type", tolower(names(response$headers)))
    size <- nchar(response$body, "bytes")
    if (is.na(idx)) {
      body <- sprintf("%d bytes of text data", size)
    } else {
      body <- sprintf("%d bytes of %s data", size, response$headers[[idx]])
    }
  }

  paste0(response$status, " with ", body)
}
