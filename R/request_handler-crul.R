#' @title RequestHandlerCrul
#' @description Methods for the crul package, building on [RequestHandler]
#' @export
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
#' 
#' # body matching
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir(), log = TRUE,
#'  log_opts = list(file = file.path(tempdir(), "vcr.log")))
#' cli <- HttpClient$new(url = "https://hb.opencpu.org")
#' 
#' ## testing, same uri and method, changed body in 2nd block
#' use_cassette(name = "apple7", {
#'   resp <- cli$post("post", body = list(foo = "bar"))
#' }, match_requests_on = c("method", "uri", "body"))
#' ## should error, b/c record="once"
#' if (interactive()) {
#'   use_cassette(name = "apple7", {
#'     resp <- cli$post("post", body = list(foo = "bar"))
#'     resp2 <- cli$post("post", body = list(hello = "world"))
#'   }, match_requests_on = c("method", "uri", "body"))
#' }
#' cas <- insert_cassette(name = "apple7", 
#'   match_requests_on = c("method", "uri", "body"))
#' resp2 <- cli$post("post", body = list(foo = "bar"))
#' eject_cassette("apple7")
#' 
#' ## testing, same body, changed method in 2nd block
#' if (interactive()) {
#' use_cassette(name = "apple8", {
#'   x <- cli$post("post", body = list(hello = "world"))
#' }, match_requests_on = c("method", "body"))
#' use_cassette(name = "apple8", {
#'   x <- cli$get("post", body = list(hello = "world"))
#' }, match_requests_on = c("method", "body"))
#' }
#' 
#' ## testing, same body, changed uri in 2nd block
#' # use_cassette(name = "apple9", {
#' #   x <- cli$post("post", body = list(hello = "world"))
#' #   w <- cli$post("get", body = list(hello = "world"))
#' # }, match_requests_on = c("method", "body"))
#' # use_cassette(name = "apple9", {
#' #   NOTHING HERE
#' # }, match_requests_on = c("method", "body"))
#' # unlink(file.path(vcr_configuration()$dir, "apple9.yml"))
#' }
RequestHandlerCrul <- R6::R6Class(
  'RequestHandlerCrul',
  inherit = RequestHandler,
  private = list(
    response_for = function(x) {
      VcrResponse$new(x$status_http(), x$response_headers,
        x$parse("UTF-8"), x$response_headers$status,
        super$cassette$cassette_opts)
    },
    on_ignored_request = function(request) {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)
      return(response)
    },

    on_stubbed_by_vcr_request = function(request) {
      # return stubbed vcr response - no real response to do
      serialize_to_crul(request, super$get_stubbed_response(request))
    },

    on_recordable_request = function(request) {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)
      self$vcr_response <- private$response_for(response)
      cas <- tryCatch(current_cassette(), error = function(e) e)
      if (inherits(cas, "error")) stop("no cassette in use")
      cas$record_http_interaction(response)
      return(response)
    }
  )
)
