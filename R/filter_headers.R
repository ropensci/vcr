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
headers_remove <- function(headers, filter) {
  is_named <- names2(filter) != ""

  to_remove <- filter[!is_named]
  for (i in seq_along(to_remove)) {
    headers[[to_remove[[i]]]] <- NULL
  }

  to_replace <- filter[is_named]
  to_replace <- to_replace[intersect(names(headers), names(to_replace))]
  for (i in seq_along(to_replace)) {
    headers[[names(to_replace)[[i]]]] <- to_replace[[i]]
  }

  headers
}

request_headers_redact <- function(headers) {
  redacted_headers <- attr(headers, "redact")
  headers[intersect(redacted_headers, names(headers))] <- "<redacted>"
  headers
}
