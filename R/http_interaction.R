#' @title HTTPInteraction class
#' @description object holds request and response objects
#' @export
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Create a hash from the HTTPInteraction object
#'     }
#'     \item{\code{from_hash(hash)}}{
#'       Create a HTTPInteraction object from a hash
#'     }
#'   }
#' @examples \dontrun{
#' # make the request
#' library(vcr)
#' url <- "https://eu.httpbin.org/post"
#' body <- list(foo = "bar")
#' cli <- crul::HttpClient$new(url = url)
#' res <- cli$post(body = body)
#'
#' # build a Request object
#' (request <- Request$new("POST", uri = url,
#'   body = body, headers = res$response_headers))
#' # build a VcrResponse object
#' (response <- VcrResponse$new(
#'    res$status_http(),
#'    res$response_headers,
#'    res$parse("UTF-8"),
#'    res$response_headers$status))
#'
#' # make HTTPInteraction object
#' (x <- HTTPInteraction$new(request = request, response = response))
#' x$recorded_at
#' x$to_hash()
#'
#' # make an HTTPInteraction from a hash with the object already made
#' x$from_hash(x$to_hash())
#'
#' # Make an HTTPInteraction from a hash alone
#' my_hash <- x$to_hash()
#' HTTPInteraction$new()$from_hash(my_hash)
#' }
HTTPInteraction <- R6::R6Class(
  'HTTPInteraction',
  public = list(
    #' @field request A `Request` class object
    request = NULL,
    #' @field response A `VcrResponse` class object
    response = NULL,
    #' @field recorded_at (character) Time http interaction recorded at 
    recorded_at = NULL,

    #' @description Create a new `HTTPInteraction` object
    #' @param request A `Request` class object
    #' @param response A `VcrResponse` class object
    #' @param recorded_at (character) Time http interaction recorded at
    #' @return A new `HTTPInteraction` object
    initialize = function(request, response, recorded_at) {
      if (!missing(request)) self$request <- request
      if (!missing(response)) self$response <- response
      self$recorded_at <- Sys.time()
    },

    #' @description Create a hash from the HTTPInteraction object
    #' @return a named list
    to_hash = function() {
      list(request = self$request$to_hash(),
           response = self$response$to_hash(),
           recorded_at = self$recorded_at)
    },

    #' @description Create a HTTPInteraction object from a hash
    #' @param hash a named list
    #' @return a new `HttpInteraction` object
    from_hash = function(hash) {
      HTTPInteraction$new(
        Request$new()$from_hash(hash$request),
        VcrResponse$new()$from_hash(hash$response),
        hash$recorded_at
      )
    }
  )
)
