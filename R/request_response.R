#' request and response summary methods
#'
#' @export
#' @name request_response
#' @keywords internal
#' @param request a [Request] object
#' @param request_matchers (character) a vector of matchers.
#' Default: `""`
#' @param response a [VcrResponse] object
#' @return character string, of either request or response
#' @details By default, method and uri are included
#' in the request summary - if body and/or headers are
#' specified in `request_matchers`, then they are also
#' included
#'
#' HTTP status code and response body are included in the
#' response summary. The response body is truncated to a
#' max of 80 characters
#' 
#' In `response_summary()` we use [gsub] with `useBytes=TRUE` to avoid
#' problems soemtimes seen with multibyte strings - this shouldn't affect
#' your data/etc. as this is only for printing a summary of the response
#' @examples
#' # request
#' url <- "https://httpbin.org"
#' body <- list(foo = "bar")
#' headers <- list(
#'   `User-Agent` = "r-curl/3.2",
#'   `Accept-Encoding` = "gzip, deflate",
#'   Accept = "application/json"
#' )
#'
#' (x <- Request$new("POST", url, body, headers))
#' request_summary(request = x)
#' request_summary(request = x, c('method', 'uri'))
#' request_summary(request = x, c('method', 'uri', 'body'))
#' request_summary(request = x, c('method', 'uri', 'headers'))
#' request_summary(request = x, c('method', 'uri', 'body', 'headers'))
#'
#' # response
#' status <- list(status_code = 200, message = "OK",
#'   explanation = "Request fulfilled, document follows")
#' headers <- list(
#'   status = "HTTP/1.1 200 OK",
#'   connection = "keep-alive",
#'   date = "Tue, 24 Apr 2018 04:46:56 GMT"
#' )
#' response_body <- 
#' "{\"args\": {\"q\": \"stuff\"}, \"headers\": {\"Accept\": \"text/html\"}}\n"
#' (x <- VcrResponse$new(status, headers,
#'    response_body, "HTTP/1.1 200 OK"))
#' response_summary(x)
#' 
#' ## with binary body
#' # path <- "tests/testthat/png_eg.rda"
#' # load(path)
#' # (x <- VcrResponse$new(status, headers, png_eg, "HTTP/1.1 200 OK"))
#' # response_summary(x)
request_summary <- function(request, request_matchers = "") {
  stopifnot(inherits(request, "Request"))
  stopifnot(inherits(request_matchers, "character"))
  atts <- c(request$method, request$uri)
  if ("body" %in% request_matchers) {
    atts <- c(atts, substring(request$body, 0, 80))
  }
  if ("headers" %in% request_matchers) {
    atts <- c(atts, request$headers)
  }
  paste0(atts, collapse = " ")
}

#' @export
#' @rdname request_response
response_summary <- function(response) {
  stopifnot(inherits(response, "VcrResponse"))
  if (inherits(response$status, c("list", "http_code"))) {
    ss <- response$status$status_code
  } else if (inherits(response$status, c("character", "integer", "numeric"))) {
    ss <- response$status
  } else {
    ss <- NULL
  }

  # if body is raw, state that it's raw
  resp <- if (is.raw(response$body)) "<raw>" else response$body

  # construct summary
  # note: gsub changes a string to UTF-8, useBytes seems to avoid doing this
  #  & avoids multibyte string errors
  sprintf("%s %s", ss %||% '???',
    substring(gsub("\n", " ", resp, useBytes = TRUE), 1, 80))
}
