#' @title The response of an HTTPInteraction
#' @description Custom vcr http response object
#' @export
#' @keywords internal
#' @examples \dontrun{
#' vcr_configure(dir = tempdir())
#'
#'
#' url <- "https://google.com"
#' (cli <- crul::HttpClient$new(url = url))
#' (res <- cli$get("get", query = list(q = "stuff")))
#' (x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    res$parse("UTF-8"), res$response_headers$status))
#' x$body
#' x$status
#' x$headers
#' x$http_version
#' x$to_hash()
#' x$from_hash(x$to_hash())
#'
#'
#' # update content length header
#' ## example 1
#' ### content-length header present, but no change
#' url <- "https://fishbase.ropensci.org"
#' cli <- crul::HttpClient$new(url = url, headers = list(`Accept-Encoding` = '*'))
#' res <- cli$get("species/34")
#' x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    res$parse("UTF-8"), res$response_headers$status)
#' x$headers$`content-length`
#' x$update_content_length_header()
#' x$headers$`content-length`
#'
#' ## example 2
#' ### no content-length header
#' url <- "https://google.com"
#' cli <- crul::HttpClient$new(url = url)
#' res <- cli$get()
#' x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    rawToChar(res$content), res$response_headers$status)
#' x$headers$`content-length`
#' x$update_content_length_header() # no change, b/c header doesn't exist
#'
#'
#' # check if body is compressed
#' url <- "https://fishbase.ropensci.org"
#' (cli <- crul::HttpClient$new(url = url))
#' (res <- cli$get("species/3"))
#' res$response_headers
#' (x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    res$parse("UTF-8"), res$response_headers$status))
#' x$content_encoding()
#' x$is_compressed()
#'
#' # with disk
#' url <- "https://google.com"
#' (cli <- crul::HttpClient$new(url = url))
#' f <- tempfile()
#' (res <- cli$get("get", query = list(q = "stuff"), disk = f))
#' (x <- VcrResponse$new(res$status_http(), res$response_headers,
#'    f, res$response_headers$status, disk = TRUE))
#' }
VcrResponse <- R6::R6Class(
  "VcrResponse",
  public = list(
    #' @field status the status of the response
    status = NULL,
    #' @field headers the response headers
    headers = NULL,
    #' @field body the response body
    body = NULL,
    #' @field http_version the HTTP version
    http_version = NULL,
    #' @field opts a list
    opts = NULL,
    #' @field adapter_metadata Additional metadata used by a specific VCR adapter
    adapter_metadata = NULL,
    #' @field hash a list
    hash = NULL,
    #' @field disk a boolean
    disk = NULL,

    #' @description Create a new VcrResponse object
    #' @param status the status of the response
    #' @param headers the response headers
    #' @param body the response body
    #' @param http_version the HTTP version
    #' @param opts a list
    #' @param adapter_metadata Additional metadata used by a specific VCR adapter
    #' @param disk boolean, is body a file on disk
    #' @return A new `VcrResponse` object
    initialize = function(status, headers, body, http_version, opts,
      adapter_metadata = NULL, disk) {
      if (!missing(status)) self$status <- status
      if (!missing(headers)) self$headers <- headers
      if (!missing(body)) {
        if (inherits(body, "list")) {
          body <- paste(names(body), body, sep = "=", collapse = ",")
        }
        self$body <- body
      }
      if (!missing(http_version)) {
        self$http_version <- extract_http_version(http_version)
      }
      if (!missing(opts)) self$opts <- opts
      if (!missing(adapter_metadata)) self$adapter_metadata <- adapter_metadata
      if (!missing(disk)) self$disk <- disk
    },

    #' @description Create a hash
    #' @return a list
    to_hash = function() {
      self$hash <- list(
        status       = self$status,
        headers      = self$headers,
        body         =
          serializable_body(self$body,
            self$opts$preserve_exact_body_bytes %||% FALSE),
        http_version = self$http_version,
        disk = self$disk
      )
      return(self$hash)
    },

    #' @description Get a hash back to an R list
    #' @param hash a list
    #' @return an `VcrResponse` object
    from_hash = function(hash) {
      VcrResponse$new(
        hash[["status"]],
        hash[["headers"]],
        body_from(hash[["body"]]),
        hash[["http_version"]],
        hash[["adapater_metadata"]],
        hash[["disk"]]
      )
    },

    #' @description Updates the Content-Length response header so that
    #' it is accurate for the response body
    #' @return no return; modifies the content length header
    update_content_length_header = function() {
      if (!is.null(self$get_header("content-length"))) {
        len <- 0
        if (length(self$body) > 0 && nchar(self$body) > 0) {
          len <- as.character(nchar(self$body))
        }
        self$edit_header("content-length", len)
      }
    },

    #' @description Get a header by name
    #' @param key (character) header name to get
    #' @return the header value (if it exists)
    get_header = function(key) {
       self$headers[[key]]
    },

    #' @description Edit a header
    #' @param key (character) header name to edit
    #' @param value (character) new value to assign
    #' @return no return; modifies the header in place
    edit_header = function(key, value = NULL) {
       self$headers[[key]] <- value
    },

    #' @description Delete a header
    #' @param key (character) header name to delete
    #' @return no return; the header is deleted if it exists
    delete_header = function(key) {
       self$headers[key] <- NULL
    },

    #' @description Get the content-encoding header value
    #' @return (character) the content-encoding value
    content_encoding = function() {
       self$get_header("content-encoding")[1]
    },

    #' @description Checks if the encoding is one of "gzip" or "deflate"
    #' @return logical
    is_compressed = function() {
       self$content_encoding() %in% c("gzip", "deflate")
    }

    # Decodes the compressed body and deletes evidence that it was ever compressed.
    # raises an error if the content encoding is not a known encoding.
    # decompress = function() {
    #   new_body <- self$decompress_(self$body, self$content_encoding())
    #   self$body <- new_body
    #   self$update_content_length_header()
    #   self$delete_header('content-encoding')
    # },

    # # used internally by `decompress()`
    # decompress_ = function(body, type) {
    #   unless HAVE_ZLIB
    #     warning("VCR: cannot decompress response; Zlib not available")
    #   end

    #   switch(
    #     type,
    #     gzip = {
    #       args = [StringIO.new(body)]
    #       args << { :encoding => 'ASCII-8BIT' } if ''.respond_to?(:encoding)
    #       yield Zlib::GzipReader.new(*args).read
    #     },
    #     deflate = yield Zlib::Inflate.inflate(body)
    #     identity = NULL,
    #     stop('UnknownContentEncodingError: unknown content encoding: ', type)
    #   )
    # }

  )
)

extract_http_version <- function(x) {
  if (!is.character(x)) return(x)
  if (grepl("HTTP/[0-9]\\.?", x)) {
    strsplit(stract(x, "HTTP/[12]\\.?([0-9])?"), "/")[[1]][2] %||% ""
  } else {
    return(x)
  }
}


# vcr_request_httr <- function(x) {
#   Request$new(
#     x$request$method,
#     x$url,
#     x$body, # FIXME: body not a field, probably index to x$request$fields
#     as.list(x$request$headers),
#     self$cassette_opts
#   )
# }

# vcr_request_crul <- function(x) {
#   Request$new(
#     x$request$method,
#     x$url,
#     x$body,
#     x$request_headers,
#     self$cassette_opts
#   )
# }



# vcr_response_httr <- function(x) {
#   VcrResponse$new(
#     httr::http_status(x),
#     x$headers,
#     httr::content(x, encoding = "UTF-8"),
#     x$all_headers[[1]]$version,
#     super$cassette$cassette_opts
#   )
# }

# vcr_response_crul <- function(x) {
#   VcrResponse$new(
#     x$status_http(),
#     headers = x$response_headers,
#     body = rawToChar(x$content),
#     http_version = x$response_headers$status,
#     self$cassette_opts
#   )
# }

