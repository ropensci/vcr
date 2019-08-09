#' RequestHandlerHttr - methods for httr package
#' @export
#' @inherit RequestHandler
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
    initialize = function(request) {
      self$request_original <- request
      self$request <- {
        Request$new(request$method, request$url,
          pluck_body(request), request$headers)
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
      response <- eval(parse(text =
        paste0("httr::", request$method)))(request$url)

      # run through response_for()
      self$vcr_response <- private$response_for(response)

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
      tmp2 <- eval(parse(text = paste0("httr::",
        self$request_original$method)))(
        self$request_original$url,
        body = get_httr_body(self$request_original)
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

get_httr_body <- function(x) {
  if (is.null(x$fields) && is.null(x$options$postfields)) return(NULL)
  if (!is.null(x$fields)) {
    form_file_comp <- vapply(x$fields, inherits, logical(1), "form_file")
    if (any(form_file_comp)) {
      ff <- x$fields[form_file_comp][[1]]
      return(ff)
    } else {
      return(x$fields)
    }
  }
  if (!is.null(x$options$postfields)) {
    if (is.raw(x$options$postfields)) return(rawToChar(x$options$postfields))
  }
}
