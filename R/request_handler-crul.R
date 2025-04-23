#' @title RequestHandlerCrul
#' @description Methods for the crul package, building on [RequestHandler]
#' @export
RequestHandlerCrul <- R6::R6Class(
  'RequestHandlerCrul',
  inherit = RequestHandler,
  private = list(
    on_ignored_request = function() {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)
      return(response)
    },

    on_stubbed_by_vcr_request = function() {
      # return stubbed vcr response - no real response to do
      serialize_to_crul(self$request, super$get_stubbed_response(self$request))
    },

    on_recordable_request = function() {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)

      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }
      current_cassette()$record_http_interaction(response)
      return(response)
    }
  )
)

# generate actual crul response
serialize_to_crul <- function(request, response) {
  # request
  req <- webmockr::RequestSignature$new(
    method = request$method,
    uri = request$uri,
    options = list(
      body = request$body %||% NULL,
      headers = request$headers %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = response$disk
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  bod <- response$body
  resp$set_body(
    if ("string" %in% names(bod)) bod$string else bod,
    response$disk %||% FALSE
  )
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  # resp$set_status(status = response$status %||% 200)
  resp$set_status(status = response$status$status_code %||% 200)

  # generate crul response
  webmockr::build_crul_response(req, resp)
}
