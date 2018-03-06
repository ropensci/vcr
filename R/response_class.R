#' The response of an HTTPInteraction
#'
#' @keywords internal
#' @param status the status of the response
#' @param headers the response headers
#' @param body the response body
#' @param http_version the HTTP version
#' @param adapter_metadata Additional metadata used by a specific VCR adapter.
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Create a hash.
#'     }
#'     \item{\code{from_hash(hash)}}{
#'       Get a hash back to an R list.
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' url <- "https://google.com"
#' (cli <- crul::HttpClient$new(url = url))
#' (res <- cli$get("search", query = list(q = "stuff")))
#' (x <- VcrResponse$new(
#'    res$status_http(),
#'    res$response_headers,
#'    res$parse("UTF-8"),
#'    res$response_headers$status))
#' x$body
#' x$status
#' x$headers
#' x$http_version
#' x$to_hash()
#' x$from_hash(x$to_hash())
#' }
VcrResponse <- R6::R6Class(
  'VcrResponse',
   public = list(
     status = NULL,
     headers = NULL,
     body = NULL,
     http_version = NULL,
     adapter_metadata = NULL,
     hash = NULL,

     initialize = function(status, headers, body, http_version, adapter_metadata = NULL) {
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
       if (!missing(adapter_metadata)) self$adapter_metadata <- adapter_metadata
     },

     to_hash = function() {
       self$hash <- list(
         status       = self$status,
         headers      = self$headers,
         body         = serializable_body(self$body),
         http_version = self$http_version
       )
       return(self$hash)
     },

     from_hash = function(hash) {
       VcrResponse$new(
         hash[['status']],
         hash[['headers']],
         body_from(hash[['body']]),
         hash[['http_version']],
         hash[['adapater_metadata']]
       )
     }
   )
)

extract_http_version <- function(x) {
  if (!is.character(x)) return(x)
  if (grepl("HTTP/[0-9]\\.?", x)) {
    strsplit(stract(x, "HTTP/[12]"), "/")[[1]][2] %||% ""
  } else {
    return(x)
  }
}
