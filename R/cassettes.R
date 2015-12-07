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
  lapply(get_cassette_meta_paths(), read_cassette_meta)
}

#' @export
#' @rdname cassettes
cassette_current <- function() last(cassettes())

#' @export
#' @rdname cassettes
cassette_path <- function() '~/vcr/vcr_cassettes'

read_cassette_meta <- function(x, ...){
  structure(yaml::yaml.load_file(x, ...), class = "cassette")
}

get_cassette_meta_paths <- function(){
  metafiles <- names(grep("metadata", sapply(cassette_files(), basename), value = TRUE))
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
  metafiles <- names(grep("metadata", sapply(cassette_files(), basename), value = TRUE))
  unname(sapply(metafiles, function(x) yaml::yaml.load_file(x)$name))
}

get_cassette_data_paths <- function(){
  files <- names(grep("metadata", sapply(cassette_files(), basename), invert = TRUE, value = TRUE))
  as.list(setNames(files, get_cassette_names()))
}

check_create_path <- function(x){
  if (file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}
