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
    },
    on_ignored_request = function() {
      httr2::local_mocked_responses(NULL)
      httr2::req_perform(self$request_original)
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      if (is.null(vcr_response$body)) {
        body <- raw()
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
      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }
      httr2::local_mocked_responses(NULL)

      req <- self$request_original
      req <- httr2::req_error(req, is_error = \(resp) FALSE)
      response <- httr2::req_perform(req)

      if (!httr2::resp_has_body(response)) {
        body <- NULL
      } else if (has_text_content(response$headers)) {
        body <- httr2::resp_body_string(response)
      } else {
        body <- httr2::resp_body_raw(response)
      }
      vcr_response <- vcr_response(
        status = response$status_code,
        headers = response$headers,
        body = body,
        # Saving body in separate file not currently supported for httr2
        disk = FALSE
      )

      current_cassette()$record_http_interaction(self$request, vcr_response)
      response
    }
  )
)

httr2_body <- function(x) {
  if (modern_httr2()) {
    if (identical(x$body$type, "file")) {
      # Preserve existing handling of file bodies
      return(x$body$data)
    } else {
      return(httr2::req_get_body(x))
    }
  }

  if (is.null(x$body)) {
    return("")
  }
  switch(
    x$body$type,
    raw = {
      # httr2::req_body_raw allows raw or string
      if (is.raw(x$body$data) && has_text_content(x$headers)) {
        rawToChar(x$body$data)
      } else {
        x$body$data
      }
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

modern_httr2 <- function() {
  exists("req_get_body", asNamespace("httr2"))
}


list2str <- function(w) {
  paste(names(w), unlist(unname(w)), sep = "=", collapse = "&")
}
