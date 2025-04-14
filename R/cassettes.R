#' List cassettes, get current cassette, etc.
#'
#' @export
#' @details
#'
#' - `cassettes()`: returns cassettes all active cassettes in the current
#'   session.
#' - `current_cassette()`: returns `NULL` when no cassettes are in use;
#' returns the current cassette (a `Cassette` object) when one is in use
#' - `cassette_path()`: returns the current directory path where cassettes
#' will be stored
#'
#' @examples
#' vcr_configure(dir = tempdir())
#'
#' # list all cassettes
#' cassettes()
#'
#' # list the currently active cassette
#' insert_cassette("stuffthings")
#' current_cassette()
#' cassettes()
#'
#' eject_cassette()
#' cassettes()
#'
#'
#' # list the path to cassettes
#' cassette_path()
#' vcr_configure(dir = file.path(tempdir(), "foo"))
#' cassette_path()
#'
#' vcr_configure_reset()
cassettes <- function() {
  cassettes_session()
}

#' @export
#' @rdname cassettes
current_cassette <- function() {
  cassettes <- cassettes()
  n <- length(cassettes)
  if (n == 0) NULL else cassettes[[n]]
}

#' @export
#' @rdname cassettes
cassette_path <- function() vcr_c$dir

cassettes_session <- function(x) {
  xx <- ls(envir = vcr_cassettes)
  if (length(xx) > 0) {
    stats::setNames(lapply(xx, get, envir = vcr_cassettes), xx)
  } else {
    list()
  }
}

include_cassette <- function(cassette) {
  # assign cassette to bucket of cassettes in session
  assign(cassette$name, cassette, envir = vcr_cassettes)
}
