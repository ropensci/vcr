#' List cassettes, get current cassette, etc.
#'
#' @export
#' @param on_disk (logical) Check for cassettes on disk + cassettes in session
#' (`TRUE`), or check for only cassettes in session (`FALSE`). Default: `TRUE`
#' @param verb (logical) verbose messages
#' @details
#'
#' - `cassettes()`: returns cassettes found in your R session, you can toggle
#' whether we pull from those on disk or not
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
#' cassettes(on_disk = FALSE)
#'
#' # list the currently active cassette
#' insert_cassette("stuffthings")
#' current_cassette()
#' eject_cassette()
#'
#' cassettes()
#' cassettes(on_disk = FALSE)
#'
#' # list the path to cassettes
#' cassette_path()
#' vcr_configure(dir = file.path(tempdir(), "foo"))
#' cassette_path()
#'
#' vcr_configure_reset()
cassettes <- function(on_disk = TRUE, verb = FALSE) {
  cassettes <- the$cassettes

  if (on_disk) {

    get_cassette_data_paths <- function() {
      files <- vcr_files()
      names <- tools::file_path_sans_ext(basename(paths))
      as.list(stats::setNames(files, get_cassette_names()))
    }

    cassettes_disk <- lapply(
      get_cassette_data_paths(),
      read_cassette_meta,
      verbose = verb
    )
    cassetes <- modifyList(cassettes, cassettes_disk)
  }

  out
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


read_cassette_meta <- function(x, verbose = TRUE, ...) {
  tmp <- yaml::yaml.load_file(x, ...)
  if (!inherits(tmp, "list") | !"http_interactions" %in% names(tmp)) {
    if (verbose) message(x, " not found, missing data, or malformed")
    return(list())
  } else {
    structure(tmp$http_interactions[[1]], class = "cassette")
  }
}


vcr_files <- function() {
  # remove some file types
  files <- names(grep(
    "metadata|rs-graphics|_pkgdown|travis|appveyor",
    vapply(cassette_files(), basename, ""),
    invert = TRUE,
    value = TRUE
  ))
  # include only certain file types
  tokeep <- switch(vcr_c$serialize_with, yaml = "yml|yaml", json = "json")
  names(grep(tokeep, vapply(cassette_files(), basename, ""), value = TRUE))
}



check_create_path <- function(x) {
  if (file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

cassette_push <- function(cassette) {
  the$cassettes[casette$name] <- cassette
  invisible(cassette)
}
cassette_pop <- function() {
  n <- length(the$cassettes)
  cassette <- cassettes[[n]]
  the$cassettes <- the$cassettes[-n]

  cassette
}
cassette_active <- function() {
  length(the$cassettes) > 0
}
