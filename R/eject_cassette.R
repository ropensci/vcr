#' Eject a cassette
#'
#' @export
#' @param options (list) a list of options to apply to the eject process
#' @param skip_no_unused_interactions_assertion (logical) If `TRUE`, this will
#' skip the "no unused HTTP interactions" assertion enabled by the
#' `allow_unused_http_interactions = FALSE` cassette option. This is intended
#' for use when your test has had an error, but your test framework has
#' already handled it - IGNORED FOR NOW
#' @return The ejected cassette if there was one
#' @seealso [use_cassette()], [insert_cassette()]
#' @examples
#' vcr_configure(dir = tempdir())
#' insert_cassette("hello")
#' (x <- current_cassette())
#'
#' # by default does current cassette
#' x <- eject_cassette()
#' x
#' # can also select by cassette name
#' # eject_cassette(cassette = "hello")
eject_cassette <- function(
  options = list(),
  skip_no_unused_interactions_assertion = NULL
) {
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
