#' Put back headers - DO WE EVEN NEED THIS?
#' @noRd
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
    torep <- freqh[nzchar(names(freqh))]
    # if character, skip, only remove in that case
    if (length(torep)) {
      for (i in seq_along(torep)) {
        x$http_interactions <- lapply(x$http_interactions, function(w) {
          if (tolower(names(torep)[i]) %in% tolower(names(w$request$headers))) {
            w$request$headers[[names(torep)[i]]] <- torep[[i]]
          }
          return(w)
        })
      }
    }
  }
  return(x)
}

#' Remove headers or replace header values
#' @noRd
#' @details
#' Applies to request and response headers.
#' @examples
#' # remove one header
#' filter_request_headers <- "User-Agent"
#' # remove multiple headers
#' filter_request_headers <- c("User-Agent", "Authorization")
#' # replace one header's value
#' filter_request_headers <- list(Authorization = "foo-bar")
#' # replace many header's values
#' filter_request_headers <- list(Authorization = "foo-bar", Accept = "everything!")
#' # mix: remove one header, replace another header's value
#' filter_request_headers <- list("Accept", Authorization = "foo-bar")
headers_remove <- function(x) {
  filter_req_or_res <- function(int, h, which) {
    if (!is.null(h)) {
      if (is.null(names(h))) toremove <- unlist(h)
      if (!is.null(names(h))) toremove <- unname(unlist(h[!nzchar(names(h))]))
      # remove zero length strings
      toremove <- Filter(nzchar, toremove)
      for (i in seq_along(toremove)) {
        int <- lapply(int, function(b) {
          b[[which]]$headers[[toremove[i]]] <- NULL
          return(b)
        })
      }

      toreplace <- h[nzchar(names(h))]
      if (length(toreplace)) {
        for (i in seq_along(toreplace)) {
          int <- lapply(int, function(b) {
            b[[which]]$headers[[names(toreplace)[i]]] <- toreplace[[i]]
            return(b)
          })
        }
      }
    }
    return(int)
  }
  x <- filter_req_or_res(x, vcr_c$filter_request_headers, "request")
  filter_req_or_res(x, vcr_c$filter_response_headers, "response")
}
