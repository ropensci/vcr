#' request and response summary methods
#' 
#' @name request_response
#' @keywords internal
#' @export
#' @param request a [Request] object
#' @param request_matchers (character) a vector of matchers
#' @param response a [VcrResponse] object
#' @return character string, of either request or response
#' @examples
#' # request
#' library("crul")
#' url <- "https://httpbin.org"
#' body <- list(foo = "bar")
#' cli <- crul::HttpClient$new(url = url)
#' res <- cli$post("post", body = body)
#'
#' (x <- Request$new("POST", url, body, res$request_headers))
#' request_summary(request = x, c('method', 'uri'))
#' request_summary(request = x, c('method', 'uri', 'body'))
#' request_summary(request = x, c('method', 'uri', 'headers'))
#' request_summary(request = x, c('method', 'uri', 'body', 'headers'))
#' 
#' # response
#' (cli <- crul::HttpClient$new(url = url))
#' (res <- cli$get("get", query = list(q = "stuff")))
#' (x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    res$parse("UTF-8"), res$response_headers$status))
#' response_summary(x)
request_summary <- function(request, request_matchers) {
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
  sprintf("%s %s", response$status$status_code, 
    substring(gsub("\n", " ", response$body), 1, 80))
}
