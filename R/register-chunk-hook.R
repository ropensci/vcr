#' Use vcr in vignettes
#'
#' @description
#' `setup_knitr()` registers a `use_cassette` hook so that you map
#' a vcr cassette to a knitr chunk. Call it in your setup chunk:
#'
#' ````
#' ```{r setup}
#' #| include: false
#' vcr::setup_knitr()
#' ```
#' ````
#'
#' Then in a chunk where you want to use a cassette, set the `cassette` chunk option
#' to the name of the cassette:
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
#'   vignettes. Defaults to the file name (sans extension) of the currently
#'   executing vignette.
#' @param dir Directory where to create the cassette file, relative to
#'   the directory containing the vignette. Default: `"_vcr"``.
#' @param ... Other arguments passed on to [insert_cassette()].
#' @export
setup_knitr <- function(prefix = NULL, dir = "_vcr", ...) {
  check_string(prefix, allow_null = TRUE)
  check_string(dir)

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
