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
            if (names(toreplace)[i] %in% names(b[[which]]$headers)) {
              b[[which]]$headers[[names(toreplace)[i]]] <- toreplace[[i]]
            }
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
