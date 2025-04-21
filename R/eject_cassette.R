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

  vcr_log_sprintf("Ejecting casette")
  cassette <- cassette_pop()
  cassette$eject()

  webmockr::disable(quiet = TRUE)
  if (!cassette_active()) {
    suppressMessages(webmockr::webmockr_disable_net_connect())
  }

  invisible(cassette)
}
