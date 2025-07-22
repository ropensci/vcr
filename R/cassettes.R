#' Manually insert and eject a cassette
#'
#' Generally you should not need to use these functions, instead preferring
#' [use_cassette()] or [local_cassette()]
#'
#' @export
#' @inheritParams use_cassette
#' @return A [Cassette], invisibly.
#' @keywords internal
#' @examples
#' vcr_configure(dir = tempdir())
#'
#' insert_cassette("hello")
#' current_cassette()
#'
#' eject_cassette()
#' current_cassette()
insert_cassette <- function(
  name,
  dir = NULL,
  record = NULL,
  match_requests_on = NULL,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  warn_on_empty = NULL
) {
  if (vcr_turned_off()) {
    return(invisible())
  }
  enable_mocks()

  # make cassette
  cassette <- Cassette$new(
    name,
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    warn_on_empty = warn_on_empty
  )
  cassette_push(cassette)
  # $insert() might error if there's a bug in the decoder, so we need to
  # make sure to unload it. We can't call `cassette_push()` after `$insert()`
  # because the active cassette name is used for logging.
  withCallingHandlers(
    cassette$insert(),
    error = function(e) cassette_pop()
  )

  invisible(cassette)
}

#' @export
#' @rdname insert_cassette
eject_cassette <- function() {
  if (!cassette_active()) {
    cli::cli_abort("No cassette in use.")
  }

  cassette_peek()$eject()
  cassette <- cassette_pop()
  if (!cassette_active()) {
    disable_mocks()
  }

  invisible(cassette)
}

#' List cassettes, get current cassette, etc.
#'
#' @export
#' @details
#'
#' - `cassettes()`: returns all active cassettes in the current
#'   session.
#' - `current_cassette()`: returns `NULL` when no cassettes are in use;
#' returns the current cassette (a `Cassette` object) when one is in use
#' - `currrent_cassette_recording()` and `current_cassette_replaying()`:
#'   tell you if the current cassette is recording and/or replaying. They
#'   both return `FALSE` if there is no cassette in use.
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
    list()
  }
}

cassette_names <- function() {
  vapply(cassettes(), function(x) x$name, character(1))
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
current_cassette_recording <- function() {
  if (cassette_active()) {
    current_cassette()$recording()
  } else {
    FALSE
  }
}

#' @export
#' @rdname cassettes
current_cassette_replaying <- function() {
  if (cassette_active()) {
    current_cassette()$replaying()
  } else {
    FALSE
  }
}

#' @export
#' @rdname cassettes
cassette_path <- function() the$config$dir

cassette_push <- function(cassette) {
  n <- length(the$cassettes)
  the$cassettes[[n + 1]] <- cassette
  invisible(cassette)
}
cassette_pop <- function() {
  n <- length(the$cassettes)
  cassette <- the$cassettes[[n]]
  the$cassettes <- the$cassettes[-n]

  cassette
}
cassette_peek <- function() {
  n <- length(the$cassettes)
  the$cassettes[[n]]
}
cassette_active <- function() {
  length(the$cassettes) > 0
}
