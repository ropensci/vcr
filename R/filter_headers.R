#' Put back headers
#' @keywords internal
#' @details
#' Applies to request headers only b/c we can not "put back"
#' response headers; that is, we can only return what response
#' headers are already in the cassette.
#' @examples
#' path <- "/Users/sckott/github/ropensci/zbank/tests/fixtures/zb_id_low_level.yml"
#' x <- yaml::yaml.load_file(path)
#' x
#' # x = list(http_interactions = list(foo = 123, bar = 456)
#' vcr_c <- new.env()
#' vcr_c$filter_request_headers <- list(Accept = "bazzle")
#' vcr_c$filter_request_headers
#' headers_put_back(x)
headers_put_back <- function(x) {
  stopifnot("x must be a list" = is.list(x))
  if (!is.null(vcr_c$filter_request_headers)) {
    freqh <- vcr_c$filter_request_headers
    # if character, skip, only remove in that case
    if (is.list(freqh)) {
      for (i in seq_along(freqh)) {
        lapply(x$http_interactions, function(w) {
          if (tolower(names(freqh)[i]) %in% tolower(names(w$request$headers))) {
            w$request$headers[[names(freqh)[i]]] <- freqh[[i]]
          }
        })
      }
    }
  }
  return(x)
}

#' Remove headers or replace header values
#' @keywords internal
#' @details
#' Applies to request and response headers.
#' @examples
#' headers_remove("")
headers_remove <- function(x) {
  fsd <- vcr_c$filter_headers
  if (!is.null(fsd)) {
    for (i in seq_along(fsd)) {
      if (nchar(fsd[[i]]) > 0) {
        x <- gsub(fsd[[i]], names(fsd)[i], x)
      }
    }
  }
  return(x)
}
