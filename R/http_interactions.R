#' Get the http interactions of the current cassette
#' 
#' @export
#' @return object of class `HTTPInteractionList` if there is a current
#' cassette in use, or `NullList` if no cassette in use
#' @examples \dontrun{
#' http_interactions()
#' }
http_interactions <- function() {
  trycurr <- tryCatch(cassette_current(), error = function(e) e)
  if (!inherits(trycurr, "error")) return(trycurr$http_interactions_)
  trycurr$http_interactions_$parent_list
}
