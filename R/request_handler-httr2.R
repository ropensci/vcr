RequestHandlerHttr2 <- R6::R6Class(
  "RequestHandlerHttr2",
  inherit = RequestHandler,

  public = list(
    initialize = function(request) {
      if (!length(request$method)) {
        request$method <- httr2_method(request)
      }
      self$request_original <- request
      self$request <- vcr_request(
        request$method,
        request$url,
        httr2_body(request),
        httr2_headers(request)
      )
    },
    on_ignored_request = function() {
      httr2::local_mocked_responses(NULL)
      if (is_streaming_request(self$request_original)) {
        httr2::req_perform_connection(self$request_original, mock = NULL)
      } else {
        httr2::req_perform(self$request_original)
      }
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      if (is.null(vcr_response$body)) {
        body <- raw()
      } else if (is.raw(vcr_response$body)) {
        body <- vcr_response$body
      } else {
        body <- charToRaw(vcr_response$body)
      }

      if (is_streaming_request(self$request_original)) {
        body <- StreamingBodyReplay$new(
          split_body_chunks(body, vcr_response$body_chunks)
        )
      }

      httr2::response(
        status_code = vcr_response$status,
        url = self$request_original$url,
        method = httr2_method(self$request_original),
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

      if (is_streaming_request(req)) {
        # We can't record a stream as the caller reads it, because we don't
        # know if they'll read all of it. So we read all of it now, using a
        # non-blocking connection so that chunks are recorded as they arrived
        # off the network, and then hand the caller a replay of what we read.
        response <- httr2::req_perform_connection(
          req,
          blocking = FALSE,
          mock = NULL
        )
        chunks <- read_stream_chunks(response$body)

        # Body is read; make a regular response to share the code below
        buffered <- response
        buffered$body <- unlist(chunks) %||% raw()
        body_chunks <- if (length(chunks) > 0) lengths(chunks)

        response$body <- StreamingBodyReplay$new(chunks)
      } else {
        response <- httr2::req_perform(req)
        buffered <- response
        body_chunks <- NULL
      }

      vcr_response <- vcr_response(
        status = buffered$status_code,
        headers = buffered$headers,
        body = httr2_response_body(buffered),
        # Saving body in separate file not currently supported for httr2
        disk = FALSE,
        body_chunks = body_chunks
      )

      current_cassette()$record_http_interaction(self$request, vcr_response)
      response
    }
  )
)

# `httr2::req_perform_connection()` adds this policy before calling the mock
is_streaming_request <- function(req) {
  isTRUE(req$policies$connection)
}

httr2_response_body <- function(response) {
  if (!httr2::resp_has_body(response)) {
    NULL
  } else if (has_text_content(response$headers)) {
    httr2::resp_body_string(response)
  } else {
    httr2::resp_body_raw(response)
  }
}

httr2_method <- function(req) {
  if (modern_httr2()) {
    return(getNamespace("httr2")$req_get_method(req))
  }

  if (!is.null(req$method)) {
    req$method
  } else if ("nobody" %in% names(req$options)) {
    "HEAD"
  } else if (!is.null(req$body)) {
    "POST"
  } else {
    "GET"
  }
}

httr2_headers <- function(req) {
  if (modern_httr2()) {
    getNamespace("httr2")$req_get_headers(req)
  } else {
    req$headers
  }
}

httr2_body <- function(x) {
  if (modern_httr2()) {
    type <- getNamespace("httr2")$req_get_body_type(x)
    data <- getNamespace("httr2")$req_get_body(x)
  } else {
    type <- x$body$type %||% "empty"
    data <- x$body$data
  }
  switch(
    type,
    # old & new
    empty = NULL,
    # old & new, but old can be string or raw
    raw = {
      if (is.raw(data) && has_text_content(x$headers)) {
        rawToChar(data)
      } else {
        data
      }
    },
    # new
    string = data,
    # old and new
    form = list2str(data),
    # old and new
    json = unclass(rlang::exec(jsonlite::toJSON, data, !!!x$body$params)),
    # old
    # FIXME: for now take the file path - would be good to get what would
    # be sent in a real request
    "raw-file" = unclass(data),
    # new
    file = unclass(data),
    # old and new
    multipart = data,
    cli::cli_abort("Unsupported request body type {.str {type}}.")
  )
}

modern_httr2 <- function() {
  exists("req_get_body", asNamespace("httr2"))
}

list2str <- function(w) {
  # TODO: replace with url_query_build() once we depend on modern httr2
  paste(names(w), unlist(unname(w)), sep = "=", collapse = "&")
}
