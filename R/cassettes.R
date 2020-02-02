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
#' - `current_cassette()`: returns an empty list when no cassettes are in use,
#' while it returns the current cassette (a `Cassette` object) when one is
#' in use
#' - `cassette_path()`: just gives you the current directory path where
#' cassettes will be stored
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
cassettes <- function(on_disk = TRUE, verb = FALSE){
  # combine cassettes on disk with cassettes in session
  if (on_disk) {
    out <- unlist(list(
      lapply(get_cassette_data_paths(), read_cassette_meta, verbose = verb),
      cassettes_session()
    ), FALSE)
    out[!duplicated(names(out))]
  } else {
    cassettes_session()
  }
}

#' @export
#' @rdname cassettes
current_cassette <- function() {
  tmp <- last(cassettes(FALSE))
  if (length(tmp) == 0) return(list())
  tmp <- if (length(tmp) == 1) tmp[[1]] else tmp
  return(tmp)
}

#' @export
#' @rdname cassettes
cassette_path <- function() vcr_c$dir

cassette_exists <- function(x) x %in% get_cassette_names()

read_cassette_meta <- function(x, verbose = TRUE, ...){
  tmp <- yaml::yaml.load_file(x, ...)
  if (!inherits(tmp, "list") | !"http_interactions" %in% names(tmp)) {
    if (verbose) message(x, " not found, missing data, or malformed")
    return(list())
  } else {
    structure(tmp$http_interactions[[1]], class = "cassette")
  }
}

get_cassette_meta_paths <- function(){
  metafiles <- names(grep("metadata", vapply(cassette_files(), basename, ""),
    value = TRUE))
  as.list(stats::setNames(metafiles, unname(sapply(metafiles, function(x)
    yaml::yaml.load_file(x)$name))))
}

cassette_files <- function(){
  path <- path.expand(cassette_path())
  check_create_path(path)
  list.files(path, full.names = TRUE)
}

get_cassette_path <- function(x){
  if ( x %in% get_cassette_names() ) get_cassette_data_paths()[[x]]
}

is_path <- function(x) file.exists(path.expand(x))

get_cassette_names <- function(){
  tmp <- vcr_files()
  if (length(tmp) == 0) return("")
  sub("\\.yml", "", basename(tmp))
}

vcr_files <- function() {
  # remove some file types
  files <- names(grep("metadata|rs-graphics",
    vapply(cassette_files(), basename, ""),
    invert = TRUE, value = TRUE))
  # include only certain file types
  # only yaml supported right now
  tokeep <- switch(vcr_c$serialize_with, yaml = "yml|yaml")
  names(grep(tokeep, vapply(cassette_files(), basename, ""),
    value = TRUE))
}

get_cassette_data_paths <- function() {
  files <- vcr_files()
  if (length(files) == 0) return(list())
  as.list(stats::setNames(files, get_cassette_names()))
}

check_create_path <- function(x){
  if (file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

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
