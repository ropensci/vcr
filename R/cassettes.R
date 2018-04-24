#' List cassettes, get current cassette, etc.
#'
#' @export
#' @param on_disk (logical) Check for cassettes on disk + cassettes in session
#' (`TRUE`), or check for only cassettes in session (`FALSE`). Default: `TRUE`
#' @param def (character) base directory for cassettes
#' @param verb (logical) verbose messages
#' @examples
#' vcr_configure("~/fixtures/vcr_cassettes")
#'
#' # list all cassettes
#' cassettes()
#' cassettes(on_disk = FALSE)
#'
#' # list the currently active cassette
#' insert_cassette("stuffthings")
#' cassette_current()
#' eject_cassette()
#'
#' # list the path to cassettes
#' cassette_path()
#' vcr_configure("foo")
#' cassette_path()
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
cassette_current <- function() {
  tmp <- last(cassettes(FALSE))
  if (length(tmp) == 0) {
    stop("there is no current cassette; insert_cassette() or use_cassette()")
  }
  tmp <- if (length(tmp) == 1) tmp[[1]] else tmp
  tmp$initialize(tmp$name)
  return(tmp)
}

#' @export
#' @rdname cassettes
cassette_path <- function(def = '~/vcr/vcr_cassettes') {
  dr <- tryCatch(vcr_configuration()$dir)
  if (inherits(dr, "error")) def else dr
}

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
  metafiles <- names(grep("metadata", vapply(cassette_files(), basename, ""), value = TRUE))
  as.list(stats::setNames(metafiles, unname(sapply(metafiles, function(x) yaml::yaml.load_file(x)$name))))
}

cassette_files <- function(){
  path <- path.expand(cassette_path())
  check_create_path(path)
  list.files(path, full.names = TRUE)
}

# get_cassette_path("foobar")
get_cassette_path <- function(x){
  if ( x %in% get_cassette_names() ) get_cassette_data_paths()[[x]]
}

is_path <- function(x) file.exists(path.expand(x))

# get_cassette_names()
get_cassette_names <- function(){
  tmp <- grep("metadata|rs-graphics", vapply(cassette_files(), basename, ""),
       invert = TRUE, value = TRUE)
  sub("\\.yml", "", basename(names(tmp)))
}

get_cassette_data_paths <- function(){
  files <- names(grep("metadata|rs-graphics", vapply(cassette_files(), basename, ""),
                      invert = TRUE, value = TRUE))
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
