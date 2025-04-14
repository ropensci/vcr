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
  if (cassette_active()) {
    the$cassettes
  } else {
    # Consistently return a named list
    set_names(list())
  }
}

#' @export
#' @rdname cassettes
current_cassette <- function() {
  if (cassette_active()) {
    n <- length(the$cassettes)
    the$cassettes[[n]]
  } else {
    NULL
  }
}

#' @export
#' @rdname cassettes
cassette_path <- function() vcr_c$dir

cassette_push <- function(cassette) {
  the$cassettes[[cassette$name]] <- cassette
  invisible(cassette)
}
cassette_pop <- function() {
  n <- length(the$cassettes)
  cassette <- the$cassettes[[n]]
  the$cassettes <- the$cassettes[-n]

  cassette
}
cassette_active <- function() {
  length(the$cassettes) > 0
}
