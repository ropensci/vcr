#' Are real http connections allowed?
#'
#' @export
#' @return boolean, `TRUE` if real HTTP requests allowed; `FALSE` if not
#' @examples
#' real_http_connections_allowed()
real_http_connections_allowed <- function() {
  trycurr <- tryCatch(current_cassette(), error = function(e) e)
  if (!inherits(trycurr, "error") && !is.null(trycurr))
    return(current_cassette()$recording())
  if (is.null(trycurr)) return(FALSE)
  !(vcr_c$allow_http_connections_when_no_cassette || !turned_on())
}
