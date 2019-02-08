#' RequestHandlerCrul - methods for crul package
#' @export
#' @inherit RequestHandler
#' @examples \dontrun{
#' vcr_configure(
#'  dir = tempdir(),
#'  record = "once"
#' )
#'
#' data(crul_request)
#' crul_request$url$handle <- curl::new_handle()
#' crul_request
#' x <- RequestHandlerCrul$new(crul_request)
#' # x$handle()
#' }
RequestHandlerCrul <- R6::R6Class(
  'RequestHandlerCrul',
  inherit = RequestHandler,
  private = list(
    # make a `vcr` response
    response_for = function(x) {
      VcrResponse$new(x$status_http(), x$response_headers,
        x$parse("UTF-8"), x$response_headers$status,
        super$cassette$cassette_opts)
    },

    # these will replace those in
    on_ignored_request = function(request) {
      # perform and return REAL http response
      # * make real request
      # * run through response_for() to make vcr response, store vcr response
      # * give back real response

      # real request
      tmp <- crul::HttpClient$new(url = request$url$url)
      tmp2 <- webmockr::webmockr_crul_fetch(request)
      response <- webmockr::build_crul_response(request, tmp2)

      # run through response_for()
      self$vcr_response <- private$response_for(response)

      # return real response
      return(response)
    },

    on_stubbed_by_vcr_request = function(request) {
      # return stubbed vcr response - no real response to do
      serialize_to_crul(request, super$get_stubbed_response(request))
    },

    on_recordable_request = function(request) {
      # do real request - then stub response - then return stubbed vcr response
      # - this may need to be called from webmockr cruladapter?

      # real request
      # tmp <- crul::HttpClient$new(url = request$url$url)
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)

      # make vcr response | then record interaction
      self$vcr_response <- private$response_for(response)
      cas <- tryCatch(current_cassette(), error = function(e) e)
      if (inherits(cas, "error")) stop("no cassette in use")
      cas$record_http_interaction(response)
      # cas$record_http_interaction(self$vcr_response)

      # return real response
      return(response)
    }
  )
)
