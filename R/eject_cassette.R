#' @export
#' @rdname insert_cassette
#' @order 2
#' @examples
#' vcr_configure(dir = tempdir())
#'
#' insert_cassette("hello")
#' current_cassette()
#'
#' eject_cassette()
#' current_cassette()
eject_cassette <- function() {
  if (!cassette_active()) {
    cli::cli_abort("No cassette in use")
  }

  cassette <- cassette_pop()
  if (!vcr_c$quiet) message("ejecting cassette: ", cassette$name)
  cassette$eject()

  webmockr::disable(quiet = vcr_c$quiet)
  if (!cassette_active()) {
    suppressMessages(webmockr::webmockr_disable_net_connect())
  }

  invisible(cassette)
}
