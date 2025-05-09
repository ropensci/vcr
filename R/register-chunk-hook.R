#' Use vcr in vignettes
#'
#' @description
#' `register_chunk_hook()` registers a `use_cassette` hook so that you map
#' a vcr cassette to a knitr chunk. Call it in your setup chunk:
#'
#' ````
#' ```{r setup}
#' #| include: false
#' vcr::register_chunk_hook()
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
#' @param prefix An optional prefix for the cassette name so that you only
#'   need to make sure that cassette names are unique within the current
#'   vignette. Default: `""``.
#' @param dir Directory where to create the cassette file. Default: `"_vcr"``.
#' @param ... Other arguments passed on to [insert_cassette()].
#' @export
register_chunk_hook <- function(prefix = "", dir = "_vcr", ...) {
  force(prefix)
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
