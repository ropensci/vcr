#' Use vcr in vignettes
#'
#' @description
#' `setup_knitr()` registers a knitr hook to make it easy to use vcr inside a
#' vignette. First call `setup_knitr()` in your setup chunk (or other chunk
#' early in the document):
#'
#' ````
#' ```{r setup}
#' #| include: false
#' vcr::setup_knitr()
#' ```
#' ````
#'
#' Then, in a chunk where you want to use a cassette, set the `cassette` chunk
#' option to the name of the cassette:
#'
#' ````
#' ```{r}
#' #| cassette: name
#' req <- httr2::request("http://r-project.org")
#' resp <- httr2::req_perform(req)
#' ```
#' ````
#'
#' @param prefix An prefix for the cassette name to make cassettes unique across
#'   vignettes. Defaults to the file name (without extension) of the currently
#'   executing vignette.
#' @param dir Directory where to create the cassette file. Default: `"_vcr"``.
#' @param ... Other arguments passed on to [insert_cassette()].
#' @export
setup_knitr <- function(prefix = NULL, dir = "_vcr", ...) {
  if (is.null(prefix)) {
    prefix <- paste0(tools::file_path_sans_ext(knitr::current_input()), "-")
  }
  dir_create(dir)

  chunk_hook <- function(before, options, name) {
    cassette_name <- options$cassette
    check_string(name)

    if (before) {
      insert_cassette(paste0(prefix, cassette_name), ..., dir = dir)
    } else {
      eject_cassette()
    }
  }

  knitr::knit_hooks$set(cassette = chunk_hook)
}
