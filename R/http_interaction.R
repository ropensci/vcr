#' @title HTTPInteraction class
#' @description object holds request and response objects
#' @export
#' @examples \dontrun{
#' # make the request
#' library(vcr)
#' url <- "https://hb.opencpu.org/post"
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
    }
  )
)
