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
    },
    on_ignored_request = function() {
      vcr_httr_mock(FALSE)
      defer(vcr_httr_mock(TRUE))

      httr_perform(self$request_original)
    },

    on_stubbed_by_vcr_request = function(vcr_response) {
      serialize_to_httr(self$request_original, vcr_response)
    },

    on_recordable_request = function() {
      vcr_httr_mock(FALSE)
      defer(vcr_httr_mock(TRUE))

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

# Helpers to create httr request from vcr request -----------------------------

httr_perform <- function(request) {
  httr::VERB(
    verb = request$method,
    url = request$url,
    body = curl_body(request),
    do.call(httr::config, request$options),
    httr::add_headers(request$headers),
    if (!is.null(request$output$path)) {
      httr::write_disk(request$output$path, TRUE)
    }
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

# Helpers to create httr response from vcr response ----------------------------

vcr_build_httr_response <- function(req, resp) {
  lst <- list(
    url = req$url,
    status_code = as.integer(resp$status_code),
    headers = {
      if (grepl("^ftp://", resp$url %||% "")) {
        # in case uri_regex only
        list()
      } else {
        hds <- resp$headers

        if (is.null(hds)) {
          hds <- resp$response_headers

          if (is.null(hds)) {
            list()
          } else {
            stopifnot(is.list(hds))
            stopifnot(is.character(hds[[1]]))
            httr::insensitive(hds)
          }
        } else {
          httr::insensitive(hds)
        }
      }
    },
    all_headers = list(),
    cookies = httr_cookies_df(),
    content = resp$content,
    date = {
      if (!is.null(resp$response_headers$date)) {
        httr::parse_http_date(resp$response_headers$date)
      } else {
        Sys.time()
      }
    },
    times = numeric(0),
    request = req,
    handle = NA
  )
  lst$all_headers <- list(list(
    status = lst$status_code,
    version = "",
    headers = lst$headers
  ))
  structure(lst, class = "response")
}

httr_cookies_df <- function() {
  df <- data.frame(matrix(ncol = 7, nrow = 0))
  x <- c("domain", "flag", "path", "secure", "expiration", "name", "value")
  colnames(df) <- x
  df
}

set_body <- function(body, disk = FALSE) {
  if (is.character(body)) {
    stopifnot(length(body) <= 1)
    if (disk) body else charToRaw(body)
  } else if (is.raw(body)) {
    body
  } else {
    raw(0)
  }
}

# generate actual httr response
serialize_to_httr <- function(httr_request, vcr_response) {
  response_body <- vcr_response$body
  if (vcr_response$disk) {
    response_body <- structure(vcr_response$body, class = "path")
  }

  resp <- list(
    url = httr_request$uri,
    status_code = vcr_response$status,
    response_headers = vcr_response$headers,
    content = set_body(response_body, vcr_response$disk)
  )

  # generate httr response
  vcr_build_httr_response(httr_request, resp)
}
