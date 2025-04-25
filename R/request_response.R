#' request and response summary methods
#'
#' @export
#' @name request_response
#' @keywords internal
#' @param request A `vcr_request` object
#' @param request_matchers (character) a vector of matchers.
#' Default: `""`
#' @param response A `vcr_response` object
#' @return character string, of either request or response
#' @details By default, method and uri are included
#' in the request summary - if body and/or headers are
#' specified in `request_matchers`, then they are also
#' included
#'
#' HTTP status code and response body are included in the
#' response summary. The response body is truncated to a
#' max of 80 characters
#'
#' In `response_summary()` we use [gsub] with `useBytes=TRUE` to avoid
#' problems sometimes seen with multibyte strings - this shouldn't affect
#' your data/etc. as this is only for printing a summary of the response
request_summary <- function(request, request_matchers = "") {
  stopifnot(inherits(request_matchers, "character"))
  atts <- c(request$method, request$uri)
  if ("body" %in% request_matchers) {
    atts <- c(atts, substring(request$body, 0, 80))
  }
  if ("headers" %in% request_matchers) {
    atts <- c(atts, request$headers)
  }
  paste0(atts, collapse = " ")
}

#' @export
#' @rdname request_response
response_summary <- function(response) {
  stopifnot(inherits(response, "vcr_response"))
  if (inherits(response$status, c("list", "http_code"))) {
    ss <- response$status$status_code
  } else if (inherits(response$status, c("character", "integer", "numeric"))) {
    ss <- response$status
  } else {
    ss <- NULL
  }

  # if body is raw, state that it's raw
  if (is.null(response$body)) {
    resp <- ""
  } else if (is.raw(response$body)) {
    resp <- "<raw>"
  } else {
    resp <- response$body
    # note: gsub changes a string to UTF-8, useBytes seems to avoid doing this
    #  & avoids multibyte string errors
    resp <- gsub("\n", " ", resp, useBytes = TRUE)
    resp <- substring(resp, 1, 80)
  }

  paste(ss %||% '???', resp)
}
