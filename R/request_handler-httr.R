#' @title RequestHandlerHttr
#' @description Methods for the httr package, building on [RequestHandler]
#' @export
#' @param request The request from an object of class `HttpInteraction`
#' @examples \dontrun{
#' vcr_configure(
#'  dir = tempdir(),
#'  record = "once"
#' )
#'
#' # GET request
#' library(httr)
#' load("~/httr_req.rda")
#' req
#' x <- RequestHandlerHttr$new(req)
#' # x$handle()
#'
#' # POST request
#' library(httr)
#' webmockr::httr_mock()
#' mydir <- file.path(tempdir(), "testing_httr")
#' invisible(vcr_configure(dir = mydir))
#' use_cassette(name = "testing2", {
#'   res <- POST("https://httpbin.org/post", body = list(foo = "bar"))
#' }, match_requests_on = c("method", "uri", "body"))
#'
#' load("~/httr_req_post.rda")
#' insert_cassette("testing3")
#' httr_req_post
#' x <- RequestHandlerHttr$new(httr_req_post)
#' x
#' # x$handle()
#' self=x
#'
#' }
RequestHandlerHttr <- R6::R6Class(
  "RequestHandlerHttr",
  inherit = RequestHandler,

  public = list(
    #' @description Create a new `RequestHandlerHttr` object
    #' @param request The request from an object of class `HttpInteraction`
    #' @return A new `RequestHandlerHttr` object
    initialize = function(request) {
      self$request_original <- request
      self$request <- {
        Request$new(request$method, request$url,
          webmockr::pluck_body(request), request$headers,
          fields = request$fields, output = request$output)
      }
      self$cassette <- tryCatch(current_cassette(), error = function(e) e)
    }
  ),

  private = list(
    # make a `vcr` response
    response_for = function(x) {
      VcrResponse$new(
        c(list(status_code = x$status_code), httr::http_status(x)),
        x$headers,
        httr::content(x, encoding = "UTF-8"),
        x$all_headers[[1]]$version,
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
      webmockr::httr_mock(FALSE)
      on.exit(webmockr::httr_mock(TRUE), add = TRUE)
      tmp2 <- eval(parse(text = paste0("httr::", request$method))) (
        request$url,
        body = webmockr::pluck_body(request),
        do.call(httr::config, request$options),
        httr::add_headers(request$headers)
      )

      # run through response_for()
      self$vcr_response <- private$response_for(tmp2)

      # return real response
      return(response)
    },

    on_stubbed_by_vcr_request = function(request) {
      # return stubbed vcr response - no real response to do
      serialize_to_httr(request, super$get_stubbed_response(request))
    },

    on_recordable_request = function(request) {
      # do real request - then stub response - then return stubbed vcr response
      # - this may need to be called from webmockr httradapter?

      # real request
      webmockr::httr_mock(FALSE)
      on.exit(webmockr::httr_mock(TRUE), add = TRUE)
      tmp2 <- eval(parse(text = paste0("httr::", self$request_original$method))) (
        self$request_original$url,
        body = webmockr::pluck_body(self$request_original),
        do.call(httr::config, self$request_original$options),
        httr::add_headers(self$request_original$headers),
        if (!is.null(self$request_original$output$path))
          httr::write_disk(self$request_original$output$path, TRUE)
      )
      response <- webmockr::build_httr_response(self$request_original, tmp2)

      # make vcr response | then record interaction
      self$vcr_response <- private$response_for(response)
      cas <- tryCatch(current_cassette(), error = function(e) e)
      if (inherits(cas, "error")) stop("no cassette in use")
      cas$record_http_interaction(response)

      # return real response
      return(response)
    }
  )
)

