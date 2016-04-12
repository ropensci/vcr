#' HTTPInteraction class
#'
#' @export
#' @docType class
#' @section Methods:
#' \describe{
#'   \item{\code{request}}{A Request class object}
#'   \item{\code{response}}{A Response class object}
#'   \item{\code{recorded_at}}{Time http interaction recorded at}
#' }
#' @examples \dontrun{
#' # make the request
#' url <- "http://httpbin.org/post"
#' body <- list(foo = "bar")
#' res <- httr::POST(url, body = body)
#'
#' # request
#' (request <- Request$new("POST", url, body, res$headers))
#' # response
#' (response <- Response$new(
#'    http_status(res),
#'    res$headers,
#'    content(res, "text"),
#'    res$all_headers[[1]]$version))
#'
#' (x <- HTTPInteraction$new(request = request, response = response))
#' x$recorded_at
#' x$to_hash()
#' x$from_hash()
#' }
HTTPInteraction <- R6::R6Class(
  'HTTPInteraction',
  public = list(
    request = NULL,
    response = NULL,
    recorded_at = NULL,

    initialize = function(request, response, recorded_at) {
      if (!missing(request)) self$request <- request
      if (!missing(response)) self$response <- response
      self$recorded_at <- Sys.time()
    },

    to_hash = function() {
      list(request = self$request$to_hash(),
           response = self$response$to_hash(),
           recorded_at = self$recorded_at)
    },

    from_hash = function() {
      list(self$request$from_hash(),
           self$response$from_hash(),
           self$recorded_at)
    }
  )
)
