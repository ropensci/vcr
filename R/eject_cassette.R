#' Eject a cassette
#'
#' @export
#' @return The ejected cassette, invisibly.
#' @seealso [use_cassette()], [insert_cassette()]
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
  cassette$eject()

  if (!cassette_active()) {
    suppressMessages(webmockr::webmockr_disable_net_connect())
  }

  invisible(cassette)
}
