#' Use a cassette to record HTTP requests
#'
#' @export
#' @param cassettes One or more cassettes as a named list 
#' @param ... a block of code containing one or more requests (required). Use
#' curly braces to encapsulate multi-line code blocks. If you can't pass a code
#' block use [insert_cassette()] instead.
#' @seealso [use_cassette()], [insert_cassette()], [eject_cassette()]
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir())
#' con <- HttpClient$new(url = "https://httpbin.org")
#'
#' use_cassette(name = "apple7", {
#'   con$get("get", query = list(a = 5))
#' })
#' use_cassette(name = "apple8", {
#'    con$get("get", query = list(b = 6))
#' })
#' 
#' cassettes <- list(list(name = "apple7"), list(name = "apple8"))
#' use_cassettes(cassettes, {
#'    con$get("get", query = list(a = 5))
#'    con$get("get", query = list(b = 6))
#' })
#' }
use_cassettes <- function(cassettes, ...) {
  cassette <- pop(cassettes)
  cassettes[[length(cassettes)]] <- NULL
  use_cassette(cassette$name, {
    if (length(cassettes) == 0) {
      force(...)
    } else {
      use_cassettes(cassettes, ...)
    }
  })
}

pop <- function(x) {
  if (length(x) == 0) {
    return(list())
  } else {
    x[length(x)][[1]]
  }
}
