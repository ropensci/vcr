#' Get the http interactions of the current cassette
#'
#' @export
#' @return object of class `HTTPInteractionList` if there is a current
#' cassette in use, or `NullList` if no cassette in use
http_interactions <- function() {
  trycurr <- tryCatch(current_cassette(), error = function(e) e)
  if (!inherits(trycurr, "error")) return(trycurr$http_interactions_)
  NullList$new()
}
