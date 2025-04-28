#' @export
RequestHandlerHttr2 <- R6::R6Class(
  "RequestHandlerHttr2",
  inherit = RequestHandler,

  public = list(
    initialize = function(request) {
      if (!length(request$method)) {
        request$method <- webmockr:::req_method_get_w(request)
      }
      self$request_original <- request
      self$request <- vcr_request(
        request$method,
        request$url,
        httr2_body(request),
        request$headers
      )
    }
  ),

  private = list(
    on_ignored_request = function() {
      httr2::local_mocked_responses(NULL)
      httr2::req_perform(self$request_original)
    },

    on_stubbed_by_vcr_request = function() {
      vcr_response <- super$get_stubbed_response(self$request)

      if (is.null(vcr_response$body)) {
        body <- NULL
      } else if (is.raw(vcr_response$body)) {
        body <- vcr_response$body
      } else {
        body <- charToRaw(vcr_response$body)
      }

      httr2::response(
        status_code = vcr_response$status,
        url = self$request_original$url,
        method = webmockr:::req_method_get_w(self$request_original),
        headers = vcr_response$headers,
        body = body
      )
    },

    on_recordable_request = function() {
      httr2::local_mocked_responses(NULL)

      req <- self$request_original
      req <- httr2::req_error(req, is_error = \(resp) FALSE)
      response <- httr2::req_perform(req)

      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }
      current_cassette()$record_http_interaction(self$request, response)
      response
    }
  )
)

httr2_body <- function(x) {
  if (is.null(x$body)) {
    return("")
  }
  switch(
    x$body$type,
    raw = {
      # httr2::req_body_raw allows raw or string
      if (is_raw(x$body$data)) rawToChar(x$body$data) else x$body$data
    },
    form = {
      data <- x$body$data # need to put back unobfuscate?
      list2str(data)
    },
    json = unclass(rlang::exec(
      jsonlite::toJSON,
      x$body$data,
      !!!x$body$params
    )),
    # FIXME: for now take the file path - would be good to get what would
    # be sent in a real request
    "raw-file" = unclass(x$body$data),
    multipart = x$body$data,
    cli::cli_abort("Unsupported request body type {.str {x$body$type}}.")
  )
}
