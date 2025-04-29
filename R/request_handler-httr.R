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
    # these will replace those in
    on_ignored_request = function() {
      # perform and return REAL http response
      # * make real request
      # * give back real response

      # real request
      webmockr::httr_mock(FALSE)
      on.exit(webmockr::httr_mock(TRUE), add = TRUE)
      tmp2 <- eval(parse(text = paste0("httr::", self$request$method)))(
        self$request$url,
        body = curl_body(self$request),
        do.call(httr::config, self$request$options),
        httr::add_headers(self$request$headers)
      )

      # return real response
      return(response)
    },

    on_stubbed_by_vcr_request = function() {
      # return stubbed vcr response - no real response to do
      serialize_to_httr(self$request, super$get_stubbed_response(self$request))
    },

    on_recordable_request = function() {
      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }

      # real request
      webmockr::httr_mock(FALSE)
      on.exit(webmockr::httr_mock(TRUE), add = TRUE)
      tmp2 <- eval(parse(
        text = paste0("httr::", self$request_original$method)
      ))(
        self$request_original$url,
        body = curl_body(self$request_original),
        do.call(httr::config, self$request_original$options),
        httr::add_headers(self$request_original$headers),
        if (!is.null(self$request_original$output$path))
          httr::write_disk(self$request_original$output$path, TRUE)
      )
      response <- webmockr::build_httr_response(self$request_original, tmp2)

      body <- vcr_body(response$content, response$headers)
      vcr_response <- vcr_response(
        status = response$status_code,
        headers = response$headers,
        body = body$body,
        disk = body$is_disk
      )

      # make vcr response | then record interaction
      current_cassette()$record_http_interaction(self$request, vcr_response)
      return(response)
    }
  )
)

vcr_body <- function(body, headers) {
  if (is.null(body)) {
    body <- NULL
    is_disk <- FALSE
  } else if (is.raw(body)) {
    if (has_binary_content(headers)) {
      body <- body
    } else {
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

# generate actual httr response
serialize_to_httr <- function(request, response) {
  # request
  req <- webmockr::RequestSignature$new(
    method = request$method,
    uri = request$uri,
    options = list(
      body = request$body %||% NULL,
      headers = request$headers %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = response$disk,
      fields = request$fields %||% NULL,
      output = request$output %||% NULL
    )
  )

  # response
  resp <- webmockr::Response$new()
  resp$set_url(request$uri)
  # in vcr >= v0.4, "disk" is in the response, but in older versions
  # its missing - use response$body if disk is not present
  response_body <- response$body
  if (response$disk) {
    response_body <- structure(response$body, class = "path")
  }
  resp$set_body(response_body, response$disk)
  resp$set_request_headers(request$headers, capitalize = FALSE)
  resp$set_response_headers(response$headers, capitalize = FALSE)
  resp$set_status(status = response$status)

  # generate httr response
  webmockr::build_httr_response(as_httr_request(req), resp)
}

keep_last <- function(...) {
  x <- c(...)
  x[!duplicated(names(x), fromLast = TRUE)]
}
httr_ops <- function(method, w) {
  # if (!length(w)) return(w)
  if (tolower(method) == "get") w$httpget <- TRUE
  if (tolower(method) == "post") w$post <- TRUE
  if (!tolower(method) %in% c("get", "post")) w$customrequest <- toupper(method)
  return(w)
}
as_httr_request <- function(x) {
  structure(
    list(
      method = toupper(x$method),
      url = x$url$url,
      headers = keep_last(x$headers),
      fields = x$fields,
      options = httr_ops(x$method, compact(keep_last(x$options))),
      auth_token = x$auth,
      output = x$output
    ),
    class = "request"
  )
}


curl_body <- function(x) {
  no_post <- (is.null(x$options$postfieldsize) || x$options$postfieldsize == 0L)
  if (is.null(x$fields) && no_post) {
    return(NULL)
  }

  if (!is.null(x$fields)) {
    # multipart body
    tmp <- x$fields
  } else if (!is.null(x$options$postfields) && is.raw(x$options$postfields)) {
    # json/raw-encoded body
    tmp <- rawToChar(x$options$postfields)
  } else if (!is.null(x$options$postfieldsize_large)) {
    # upload not in a list
    # seems like we can't get the file path anyway from the request
    # in both crul and httr - so may be stuck with this
    tmp <- paste0("upload, file size: ", x$options$postfieldsize_large)
  } else {
    # unknown, fail out
    cli::cli_abort("couldn't fetch request body; please file an issue")
  }
  if (inherits(tmp, "raw")) rawToChar(tmp) else tmp
}

is_body_empty <- function(x) {
  is.null(x$fields) &&
    (is.null(x$options$postfieldsize) || x$options$postfieldsize == 0L)
}

save_file <- function(path) {
  if (is.null(vcr_c$write_disk_path)) {
    cli::cli_abort(c(
      "`write_disk_path` must be set when writing to disk.",
      i = "See ?vcr_configure for details."
    ))
  }
  out_path <- file.path(vcr_c$write_disk_path, basename(path))

  file.copy(path, out_path, overwrite = TRUE)
  out_path
}
