#' Get the http interactions of the current cassette
#'
#' @export
#' @return object of class `HTTPInteractionList` if there is a current
#' cassette in use, or `NullList` if no cassette in use
#' @examples \dontrun{
#' vcr_configure(dir = tempdir())
#' insert_cassette("foo_bar")
#' webmockr::webmockr_allow_net_connect()
#' library(crul)
#' cli <- crul::HttpClient$new("https://hb.opencpu.org/get")
#' one <- cli$get(query = list(a = 5))
#' z <- http_interactions()
#' z
#' z$interactions
#' z$used_interactions
#' # on eject, request written to the cassette
#' eject_cassette()
#'
#' # insert cassette again
#' insert_cassette("foo_bar")
#' # now interactions will be present
#' z <- http_interactions()
#' z$interactions
#' z$used_interactions
#' invisible(cli$get(query = list(a = 5)))
#' z$used_interactions
#'
#' # cleanup
#' unlink(file.path(tempdir(), "foo_bar.yml"))
#' }
http_interactions <- function() {
  trycurr <- tryCatch(current_cassette(), error = function(e) e)
  if (!inherits(trycurr, "error")) return(trycurr$http_interactions_)
  NullList$new()
}
