#' @title RequestHandlerHttr2
#' @description Methods for the httr2 package, building on [RequestHandler]
#' @export
#' @param request The request from.
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
    #' @param request The request
    #' @return A new `RequestHandlerHttr2` object
    initialize = function(request) {
      if (!length(request$method)) {
        request$method <- webmockr:::req_method_get_w(request)
      }
      self$request_original <- request
      self$request <- {
        Request$new(
          request$method,
          request$url,
          take_body(request),
          request$headers,
          fields = request$fields,
          policies = request$policies
        )
      }
    }
  ),

  private = list(
    # these will replace those in
    on_ignored_request = function() {
      # perform and return REAL http response
      # * make real request
      # * give back real response

      # real request
      webmockr::httr2_mock(FALSE)
      on.exit(webmockr::httr2_mock(TRUE), add = TRUE)
      response <- httr2::req_perform(self$request)

      # return real response
      return(response)
    },

    on_stubbed_by_vcr_request = function() {
      # print("------- on_stubbed_by_vcr_request -------")
      # return stubbed vcr response - no real response to do
      serialize_to_httr2(self$request, super$get_stubbed_response(self$request))
    },

    on_recordable_request = function() {
      # print("------- on_recordable_request -------")
      # do real request - then stub response - then return stubbed vcr response
      # real request
      webmockr::httr2_mock(FALSE)
      on.exit(webmockr::httr2_mock(TRUE), add = TRUE)
      xx <- httr2::req_error(
        self$request_original,
        is_error = function(resp) FALSE
      )
      # print(xx)
      tryCatch(httr2::req_perform(xx), error = function(e) e)
      tmp2 <- httr2::last_response()
      # print("------- after the req_perform -------")

      response <- webmockr::build_httr2_response(self$request_original, tmp2)

      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }
      response$request <- self$request_original
      response$request$method <- webmockr:::req_method_get_w(response$request)
      current_cassette()$record_http_interaction(response)

      # return real response
      # print("------- before return -------")
      return(response)
    }
  )
)

# generate actual httr2 response
# request <- httr2::request("https://hb.opencpu.org/301")
#
serialize_to_httr2 <- function(request, response) {
  # request
  req <- webmockr::RequestSignature$new(
    method = request$method,
    uri = request$uri,
    options = list(
      body = request$body %||% NULL,
      headers = request$headers %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = if (inherits(response$body, "httr2_path")) response$body %||% NULL,
      fields = request$fields %||% NULL,
      output = request$output %||% NULL
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  resp$set_body(response$body, inherits(response$body, "httr2_path"))
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate httr2 response
  webmockr::build_httr2_response(as_httr2_request(req), resp)
}

as_httr2_request <- function(x) {
  structure(
    list(
      url = x$url$url,
      method = toupper(x$method),
      headers = x$headers,
      body = x$body,
      fields = x$fields,
      options = x$options,
      policies = x$policies
    ),
    class = "httr2_request"
  )
}
