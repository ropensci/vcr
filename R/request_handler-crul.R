RequestHandlerCrul <- R6::R6Class(
  'RequestHandlerCrul',
  inherit = RequestHandler,
  public = list(
    initialize = function(request) {
      self$request_original <- request

      self$request <- vcr_request(
        request$method,
        request$url$url,
        curl_body(request),
        as.list(request$headers)
      )
    },
    on_ignored_request = function() {
      tmp2 <- vcr_crul_fetch(self$request_original)
      response <- vcr_build_crul_response(self$request_original, tmp2)
      return(response)
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      response <- serialize_to_crul(self$request, vcr_response)
      if (is.character(response$content) && vcr_response$disk) {
        response <- update_vcr_disk_path(response)
      }
      return(response)
    },

    on_recordable_request = function() {
      if (!cassette_active()) {
        cli::cli_abort("No cassette in use.")
      }

      tmp2 <- vcr_crul_fetch(self$request_original)
      response <- vcr_build_crul_response(self$request_original, tmp2)

      body <- vcr_body(response$content, response$response_headers)
      vcr_response <- vcr_response(
        status = as.integer(response$status_http()$status_code),
        headers = response$response_headers,
        body = body$body,
        disk = body$is_disk
      )

      current_cassette()$record_http_interaction(self$request, vcr_response)
      if (is.character(response$content)) {
        if (file.exists(response$content)) {
          response <- update_vcr_disk_path(response)
        }
      }
      return(response)
    }
  )
)

update_vcr_disk_path <- function(response) {
  if (is.null(the$config$write_disk_path)) {
    abort(c(
      "if writing to disk, write_disk_path must be given",
      "see ?vcr::vcr_configure"
    ))
  }

  response$content <- file.path(
    the$config$write_disk_path,
    basename(response$content)
  )
  response$request$disk <- response$content
  response
}

# generate actual crul response
serialize_to_crul <- function(request, response) {
  # request
  req <- list(
    method = request$method,
    uri = request$uri,
    headers = request$headers %||% NULL,
    options = list(
      body = request$body %||% NULL,
      proxies = NULL,
      auth = NULL,
      disk = response$disk
    )
  )

  # response
  body <- set_body(response$body, response$disk)
  resp <- list(
    url = req$uri,
    status_code = response$status,
    response_headers = response$headers,
    content = body,
    body = body
  )

  # generate crul response
  vcr_build_crul_response(req, resp)
}

vcr_build_crul_response <- function(req, resp) {
  # prep headers
  if (grepl("^ftp://", resp$url %||% "")) {
    # in case uri_regex only
    headers <- list()
  } else {
    hds <- resp$headers
    if (is.null(hds)) {
      hds <- resp$response_headers
      headers <- if (is.null(hds)) {
        list()
      } else {
        stopifnot(is.list(hds))
        stopifnot(is.character(hds[[1]]))
        hds
      }
    } else {
      hh <- rawToChar(hds %||% raw(0))
      if (is.null(hh) || nchar(hh) == 0) {
        headers <- list()
      } else {
        headers <- lapply(
          curl::parse_headers(hh, multiple = TRUE),
          crul_headers_parse
        )
      }
    }
  }

  crul::HttpResponse$new(
    method = req$method,
    # if resp URL is empty, use URL from request
    url = resp$url %||% req$url$url,
    status_code = resp$status_code,
    request_headers = as.list(c(
      "User-Agent" = req$options$useragent,
      req$headers
    )),
    response_headers = {
      if (all(hz_namez(headers))) headers else headers[[length(headers)]]
    },
    response_headers_all = headers,
    modified = resp$modified %||% NA,
    times = resp$times,
    content = resp$content %||% resp$body,
    handle = req$url$handle,
    request = req
  )
}

vcr_crul_fetch <- function(x) {
  if (is_null(x$disk) && is_null(x$stream)) {
    curl::curl_fetch_memory(x$url$url, handle = x$url$handle)
  } else if (!is_null(x$disk)) {
    curl::curl_fetch_disk(x$url$url, x$disk, handle = x$url$handle)
  } else {
    curl::curl_fetch_stream(x$url$url, x$stream, handle = x$url$handle)
  }
}

crul_head_parse <- function(z) {
  if (grepl("HTTP\\/", z)) {
    list(status = z)
  } else {
    ff <- regexec("^([^:]*):\\s*(.*)$", z)
    xx <- regmatches(z, ff)[[1]]
    as.list(stats::setNames(xx[[3]], tolower(xx[[2]])))
  }
}

crul_headers_parse <- function(x) do.call("c", lapply(x, crul_head_parse))
