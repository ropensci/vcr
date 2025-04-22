#' Are real http connections allowed?
#'
#' @export
#' @return boolean, `TRUE` if real HTTP requests allowed; `FALSE` if not
#' @examples
#' real_http_connections_allowed()
real_http_connections_allowed <- function() {
  if (cassette_active()) {
    current_cassette()$recording()
  } else {
    FALSE
  }
}
