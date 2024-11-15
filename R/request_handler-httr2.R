#' @title RequestHandlerHttr2
#' @description Methods for the httr2 package, building on [RequestHandler]
#' @export
#' @param request The request from an object of class `HttpInteraction`
#' @examples \dontrun{
#' # GET request
#' library(httr2)
#' req <- request("https://hb.opencpu.org/post") %>%
#'    req_body_json(list(foo = "bar"))
#' x <- RequestHandlerHttr2$new(req)
#' # x$handle()
#'
#' # POST request
#' library(httr2)
#' mydir <- file.path(tempdir(), "testing_httr2")
#' invisible(vcr_configure(dir = mydir))
#' req <- request("https://hb.opencpu.org/post") %>%
#'   req_body_json(list(foo = "bar"))
#' use_cassette(name = "testing3", {
#'   response <- req_perform(req)
#' }, match_requests_on = c("method", "uri", "body"))
#' use_cassette(name = "testing3", {
#'   response2 <- req_perform(req)
#' }, match_requests_on = c("method", "uri", "body"))
#' }
RequestHandlerHttr2 <- R6::R6Class(
  "RequestHandlerHttr2",
  inherit = RequestHandler,

  public = list(
    #' @description Create a new `RequestHandlerHttr2` object
    #' @param request The request from an object of class `HttpInteraction`
    #' @return A new `RequestHandlerHttr2` object
    initialize = function(request) {
      if (!length(request$method)) {
        request$method <- webmockr:::req_method_get_w(request)
      }
      self$request_original <- request
      self$request <- {
        Request$new(request$method, request$url,
          webmockr::pluck_body(request), request$headers,
          fields = request$fields, opts = request$options,
          policies = request$policies)
      }
      self$cassette <- tryCatch(current_cassette(), error = function(e) e)
    }
  ),

  private = list(
    # make a `vcr` response
    response_for = function(x) {
      VcrResponse$new(
        list(status_code = x$status_code, description = httr2::resp_status_desc(x)),
        x$headers,
        x$body,
        "",
        super$cassette$cassette_opts
      )
    },

    # these will replace those in
    on_ignored_request = function(request) {
      # perform and return REAL http response
      # * make real request
      # * run through response_for() to make vcr response, store vcr response
      # * give back real response

      # real request
      webmockr::httr2_mock(FALSE)
      on.exit(webmockr::httr2_mock(TRUE), add = TRUE)
      tmp2 <- httr2::req_perform(request)

      # run through response_for()
      self$vcr_response <- private$response_for(tmp2)

      # return real response
      return(response)
    },

    on_stubbed_by_vcr_request = function(request) {
      # print("------- on_stubbed_by_vcr_request -------")
      # return stubbed vcr response - no real response to do
      serialize_to_httr2(request, super$get_stubbed_response(request))
    },

    on_recordable_request = function(request) {
      # print("------- on_recordable_request -------")
      # do real request - then stub response - then return stubbed vcr response
      # real request
      webmockr::httr2_mock(FALSE)
      on.exit(webmockr::httr2_mock(TRUE), add = TRUE)
      xx <- self$request_original %>% 
        httr2::req_error(is_error = function(resp) FALSE)
      # print(xx)
      tryCatch(httr2::req_perform(xx), error = function(e) e)
      tmp2 <- httr2::last_response()
      # print("------- after the req_perform -------")

      response <- webmockr::build_httr2_response(self$request_original, tmp2)

      # make vcr response | then record interaction
      self$vcr_response <- private$response_for(response)
      cas <- tryCatch(current_cassette(), error = function(e) e)
      if (inherits(cas, "error")) stop("no cassette in use")
      response$request <- self$request_original
      response$request$method <- webmockr:::req_method_get_w(response$request)
      cas$record_http_interaction(response)

      # return real response
      # print("------- before return -------")
      return(response)
    }
  )
)

