#' @export
RequestHandlerCrul <- R6::R6Class(
  'RequestHandlerCrul',
  inherit = RequestHandler,
  public = list(
    initialize = function(request) {
      self$request_original <- request

      fake_resp <- webmockr::build_crul_response(request, NULL)
      self$request <- vcr_request(
        request$method,
        request$url$url,
        curl_body(request),
        as.list(fake_resp$request_headers)
      )
    },
    on_ignored_request = function() {
      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)
      return(response)
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      # return stubbed vcr response - no real response to do
      serialize_to_crul(self$request, vcr_response)
    },

    on_recordable_request = function() {
      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }

      tmp2 <- webmockr::webmockr_crul_fetch(self$request_original)
      response <- webmockr::build_crul_response(self$request_original, tmp2)

      body <- vcr_body(response$content, response$response_headers)
      vcr_response <- vcr_response(
        status = as.integer(response$status_http()$status_code),
        headers = response$response_headers,
        body = body$body,
        disk = body$is_disk
      )

      current_cassette()$record_http_interaction(self$request, vcr_response)
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
  resp$set_body(response$body, response$disk)
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  # resp$set_status(status = response$status %||% 200)
  resp$set_status(status = response$status)

  # generate crul response
  webmockr::build_crul_response(req, resp)
}
