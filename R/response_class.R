#' @title The response of an HTTPInteraction
#' @description Custom vcr http response object
#' @export
#' @keywords internal
#' @examples \dontrun{
#' vcr_configure(dir = tempdir())
#'
#' # basic example of VcrResponse use
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
    initialize = function(
      status,
      headers,
      body,
      http_version,
      opts,
      adapter_metadata = NULL,
      disk
    ) {
      if (!missing(status)) self$status <- status
      if (!missing(headers)) self$headers <- headers
      if (!missing(body)) {
        if (inherits(body, "list")) {
          body <- paste(names(body), body, sep = "=", collapse = ",")
        }
        # self$body <- if (is.character(body)) enc2utf8(body) else body
        self$body <- body
      }
      if (!missing(http_version)) {
        self$http_version <- extract_http_version(http_version)
      }
      if (!missing(opts)) self$opts <- opts
      if (!missing(adapter_metadata)) self$adapter_metadata <- adapter_metadata
      if (!missing(disk)) self$disk <- disk
    },

    #' @description print method for the `VcrResponse` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) cat("<VcrResponse> ", sep = "\n"),

    #' @description Create a hash
    #' @return a list
    to_hash = function() {
      list(
        status = self$status,
        headers = self$headers,
        body = self$body,
        http_version = self$http_version,
        disk = self$disk
      )
    },

    #' @description Get a hash back to an R list
    #' @param hash a list
    #' @return an `VcrResponse` object
    from_hash = function(hash) {
      VcrResponse$new(
        hash[["status"]],
        hash[["headers"]],
        hash[["body"]] %||% "",
        hash[["http_version"]],
        hash[["adapater_metadata"]],
        hash[["disk"]]
      )
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
