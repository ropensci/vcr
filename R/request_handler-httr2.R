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
          request$headers
        )
      }
    }
  ),

  private = list(
    on_ignored_request = function() {
      httr2::local_mocked_responses(NULL)
      httr2::req_perform(self$request_original)
    },

    on_stubbed_by_vcr_request = function() {
      serialize_to_httr2(self$request, super$get_stubbed_response(self$request))
    },

    on_recordable_request = function() {
      httr2::local_mocked_responses(NULL)

      req <- self$request_original
      req <- httr2::req_error(req, is_error = \(resp) FALSE)
      resp <- httr2::req_perform(req)

      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }
      current_cassette()$record_http_interaction(resp)
      resp
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
  resp <- webmockr::build_httr2_response(as_httr2_request(req), resp)
  attr(resp$headers, "redact") <- character()
  resp
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

#' @note adapted from httr2:::req_body_get
#' @keywords internal
take_body.httr2_request <- function(x) {
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
      httr2_url_build(data)
    },
    json = rlang::exec(jsonlite::toJSON, x$body$data, !!!x$body$params),
    # FIXME: for now take the file path - would be good to get what would
    # be sent in a real request
    "raw-file" = x$body$data,
    multipart = x$body$data,
    cli::cli_abort("Unsupported request body type {.str {x$body$type}}.")
  )
}
