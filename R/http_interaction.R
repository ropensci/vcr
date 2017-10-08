#' HTTPInteraction class
#'
#' @export
#' @param request A `Request` class object
#' @param response A `VcrResponse` class object
#' @param recorded_at Time http interaction recorded at
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Create a hash.
#'     }
#'     \item{\code{from_hash()}}{
#'       Get a hash back to an R list.
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' # make the request
#' url <- "https://httpbin.org/post"
#' body <- list(foo = "bar")
#' cli <- crul::HttpClient$new(url = url)
#' res <- cli$post(body = body)
#'
#' # request
#' (request <- Request$new("POST", uri = url,
#'   body = body, headers = res$response_headers))
#' # response
#' (response <- VcrResponse$new(
#'    res$status_http(),
#'    res$response_headers,
#'    res$parse("UTF-8"),
#'    res$response_headers$status))
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
    }
#
#     from_hash = function() {
#       list(self$request$from_hash(),
#            self$response$from_hash(),
#            self$recorded_at)
#     }
  )
)
