#' Eject a cassette
#'
#' @export
#' @param cassette (character) a single cassette names to eject
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
eject_cassette <- function(cassette = NULL, options = list(),
                           skip_no_unused_interactions_assertion = NULL) {
  if (is.null(cassette)) {
    # current cassette
    cas <- current_cassette()
    if (length(cas) == 0) stop("no cassette in use currently", call. = FALSE)
  } else {
    if (!cassette_exists(cassette)) {
      stop("cassette '", cassette, "' not found", call. = FALSE)
    }
    cas <- cassettes(FALSE)[[cassette]]
    if (is.null(cas)) stop("cassette '", cassette, "' not found", call. = FALSE)
  }
  # eject it
  cas$eject()
}
