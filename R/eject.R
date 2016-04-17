#' Eject a cassette
#'
#' @export
#' @param cassette (character) One or more cassettes to eject
#' @param options Optional list of options to pass to eject
#' @examples \dontrun{
#' x <- insert_cassette(name = "fartloud")
#' eject(x)
#' }
eject <- function(cassette, options = list()) {
  cassette$eject()
}
