#' List cassettes, get current cassette, etc.
#'
#' @export
#' @examples \dontrun{
#' # list all cassettes
#' cassettes()
#'
#' # list the currently active cassette
#' cassette_current()
#'
#' # list the path to cassettes
#' cassette_path()
#' }
cassettes <- function(){
  # combine cassettes on disk with cassettes in session
  out <- unlist(list(
    lapply(get_cassette_meta_paths(), read_cassette_meta),
    cassettes_session()
  ), FALSE)
  out[!duplicated(names(out))]
}

#' @export
#' @rdname cassettes
cassette_current <- function() last(cassettes())

#' @export
#' @rdname cassettes
cassette_path <- function() '~/vcr/vcr_cassettes'

cassette_exists <- function(x) x %in% get_cassette_names()

read_cassette_meta <- function(x, ...){
  structure(yaml::yaml.load_file(x, ...), class = "cassette")
}

get_cassette_meta_paths <- function(){
  metafiles <- names(grep("metadata", vapply(cassette_files(), basename, ""), value = TRUE))
  as.list(setNames(metafiles, unname(sapply(metafiles, function(x) yaml::yaml.load_file(x)$name))))
}

cassette_files <- function(){
  path <- path.expand(cassette_path())
  check_create_path(path)
  list.files(path, full.names = TRUE)
}

# get_cassette_path("foobar")
get_cassette_path <- function(x){
  if ( x %in% get_cassette_names() ) get_cassette_meta_paths()[[x]]
}

is_path <- function(x) file.exists(path.expand(x))

# get_cassette_names()
get_cassette_names <- function(){
  metafiles <- names(grep("metadata", vapply(cassette_files(), basename, ""), value = TRUE))
  unname(sapply(metafiles, function(x) yaml::yaml.load_file(x)$name))
  #vapply(strsplit(unname(vapply(cassette_files(), basename, "")), "\\."), "[[", "", 1)
}

get_cassette_data_paths <- function(){
  files <- names(grep("metadata", vapply(cassette_files(), basename, ""), invert = TRUE, value = TRUE))
  as.list(setNames(files, get_cassette_names()))
}

check_create_path <- function(x){
  if (file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

cassettes_session <- function(x) {
  xx <- ls(envir = vcr_cassettes)
  if (length(xx) > 0) {
    setNames(lapply(xx, get, envir = vcr_cassettes), xx)
  } else {
    list()
  }
}

include_cassette <- function(cassette) {
  # assign cassette to bucket of cassettes in session
  assign(cassette$name, cassette, envir = vcr_cassettes)
}
