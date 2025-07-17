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
#' Or if you have labelled the chunk, you can use `cassette: true` to use the
#' chunk label as the cassette name:
#'
#' ````
#' ```{r}
#' #| label: cassette-name
#' #| cassette: true
#' req <- httr2::request("http://r-project.org")
#' resp <- httr2::req_perform(req)
#' ```
#' ````
#'
#' You can build your vignettes however you usually do (from the command line,
#' with pkgdown, with RStudio/Positron, etc).
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

  knitr::knit_hooks$set(cassette = vcr_hook(prefix, dir, ...))
}

vcr_hook <- function(prefix, dir, ...) {
  force(prefix)
  force(dir)

  function(before, options, name) {
    if (!before) {
      return(eject_cassette())
    }

    cassette_name <- options$cassette
    if (isTRUE(cassette_name)) {
      cassette_name <- options$label
    } else if (is_string(cassette_name)) {
      # ok
    } else {
      stop_input_type(
        cassette_name,
        c("a string", "TRUE"),
        arg = "cassette",
        call = quote(`options$cassette`())
      )
    }

    insert_cassette(paste0(prefix, cassette_name), ..., dir = dir)
  }
}
