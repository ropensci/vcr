#' @export
RequestHandlerHttr <- R6::R6Class(
  "RequestHandlerHttr",
  inherit = RequestHandler,

  public = list(
    initialize = function(request) {
      self$request_original <- request
      self$request <- vcr_request(
        request$method,
        request$url,
        curl_body(request),
        as.list(request$headers)
      )
    }
  ),

  private = list(
    on_ignored_request = function() {
      webmockr::httr_mock(FALSE)
      withr::defer(webmockr::httr_mock(TRUE))

      httr_perform(self$request_original)
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      serialize_to_httr(self$request_original, vcr_response)
    },

    on_recordable_request = function() {
      webmockr::httr_mock(FALSE)
      withr::defer(webmockr::httr_mock(TRUE))

      response <- httr_perform(self$request_original)

      body <- vcr_body(response$content, response$headers)
      vcr_response <- vcr_response(
        status = response$status_code,
        headers = response$headers,
        body = body$body,
        disk = body$is_disk
      )

      current_cassette()$record_http_interaction(self$request, vcr_response)
      return(response)
    }
  )
)


# generate actual httr response
serialize_to_httr <- function(httr_request, vcr_response) {
  resp <- webmockr::Response$new()
  resp$set_url(httr_request$uri)
  # in vcr >= v0.4, "disk" is in the response, but in older versions
  # its missing - use response$body if disk is not present
  response_body <- vcr_response$body
  if (vcr_response$disk) {
    response_body <- structure(vcr_response$body, class = "path")
  }
  resp$set_body(response_body, vcr_response$disk)
  resp$set_request_headers(httr_request$headers, capitalize = FALSE)
  resp$set_response_headers(vcr_response$headers, capitalize = FALSE)
  resp$set_status(status = vcr_response$status)

  # generate httr response
  webmockr::build_httr_response(httr_request, resp)
}

# Helpers to create httr request from vcr request -----------------------------

httr_perform <- function(request) {
  httr::VERB(
    verb = request$method,
    url = request$url,
    body = curl_body(request),
    do.call(httr::config, request$options),
    httr::add_headers(request$headers),
    if (!is.null(request$output$path))
      httr::write_disk(request$output$path, TRUE)
  )
}

curl_body <- function(x) {
  no_post <- (is.null(x$options$postfieldsize) || x$options$postfieldsize == 0L)
  if (is.null(x$fields) && no_post) {
    return(NULL)
  }

  if (!is.null(x$fields)) {
    # multipart body
    x$fields
  } else if (!is.null(x$options$postfields) && is.raw(x$options$postfields)) {
    # json/raw-encoded body
    rawToChar(x$options$postfields)
  } else if (!is.null(x$options$postfieldsize_large)) {
    # upload not in a list
    # seems like we can't get the file path anyway from the request
    # in both crul and httr - so may be stuck with this
    paste0("upload, file size: ", x$options$postfieldsize_large)
  } else {
    # unknown, fail out
    cli::cli_abort("couldn't fetch request body; please file an issue")
  }
}

# Helpers to create vcr response from httr response ----------------------------

vcr_body <- function(body, headers) {
  if (is.null(body)) {
    body <- NULL
    is_disk <- FALSE
  } else if (is.raw(body)) {
    if (has_text_content(headers)) {
      body <- rawToChar(body)
    }
    is_disk <- FALSE
  } else if (inherits(body, "path") || (is_string(body) && file.exists(body))) {
    body <- save_file(body)
    is_disk <- TRUE
  } else {
    cli::cli_abort("Unrecognized response content type.", .internal = TRUE)
  }

  list(body = body, is_disk = is_disk)
}
save_file <- function(path) {
  basepath <- the$config$write_disk_path
  if (is.null(basepath)) {
    basepath <- file.path(
      the$config$dir,
      paste0(current_cassette()$name, "-files")
    )
  }
  dir_create(basepath)

  out_path <- file.path(basepath, basename(path))
  file.copy(path, out_path, overwrite = TRUE)
  out_path
}
