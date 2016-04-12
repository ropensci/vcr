#' Eject a cassette
#'
#' @export
#' @param cassettes (character) One or more cassettes to eject
#' @param options Optional list of options
#' @examples \dontrun{
#' res <- Cassette$new("foobar")
#'
#' x <- cassettes()
#' (cas <- as.cassette(x[[1]]))
#' as.cassette(cas)
#' as.cassette(cassettes()[[1]])
#' as.cassette("foobar")
#'
#' insert_cassette(name = "fartloud")
#'
#' use_cassette("foobar", GET("http://google.com"))
#'
#' eject_cassette("foobar")
#' }
eject_cassette <- function(cassettes, options = list()) {
  # Cassette$eject()
  cassette <- last(cassettes())
  # cassette.eject(options) # if cassette use cassette_eject()
  # cassette
  # ensure
  #   cassettes.pop
}
