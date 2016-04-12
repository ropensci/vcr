#' The request of an HTTPInteraction
#'
#' @keywords internal
#' @param method the HTTP method (i.e. :head, :options, :get, :post, :put, :patch or :delete)
#' @param uri the request URI
#' @param body the request body
#' @param headers the request headers
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Create a hash.
#'     }
#'     \item{\code{from_hash()}}{
#'       Get a hash back to an R list.
#'     }
#'   }
#' @examples \dontrun{
#' url <- "http://httpbin.org/post"
#' body <- list(foo = "bar")
#' res <- httr::POST(url, body = body)
#' (x <- Request$new("POST", url, body, res$headers))
#' x$body
#' x$method
#' x$uri
#' x$headers
#' x$to_hash()
#' x$from_hash()
#'
#' vcr_configure(
#'  dir = "fixtures/vcr_cassettes",
#'  record = "once",
#'  preserve_exact_body_bytes_for = TRUE
#' )
#' }
Request <- R6::R6Class('Request',
   public = list(
     method = NULL,
     uri = NULL,
     body = NULL,
     headers = NULL,
     skip_port_stripping = FALSE,
     hash = NULL,

     initialize = function(method, uri, body, headers) {
       if (!missing(method)) self$method <- tolower(method)
       if (!missing(body)) {
         if (inherits(body, "list")) {
           body <- paste(names(body), body, sep = "=", collapse = ",")
         }
         self$body <- body
       }
       if (!missing(headers)) self$headers <- headers
       if (!missing(uri)) {
         if (!self$skip_port_stripping) {
           self$uri <- private$without_standard_port(uri)
         } else {
           self$uri <- uri
         }
       }
     },

     to_hash = function() {
       self$hash <- list(
         method  = self$method,
         uri     = self$uri,
         body    = serializable_body(self$body),
         headers = self$headers
       )
       return(self$hash)
     },

     from_hash = function() {
       method <- self$hash[['method']]
       list(
         self$hash[['uri']],
         body_from(self$hash[['body']]),
         self$hash[['headers']],
         self$skip_port_stripping
       )
     }
   ),
   private = list(
     without_standard_port = function(uri) {
       if (is.null(uri)) return(uri)
       u <- private$parsed_uri(uri)
       if (paste0(u$scheme, u$port) %in% c('http', 'https/443')) {
         return(uri)
       }
       u$port = NULL
       # FIXME, don't love that I have to call build_url here
       return(httr::build_url(u))
     },

     parsed_uri = function(uri) {
       eval(parse(text = vcr_configuration()$uri_parser))(uri)
     }
   )
)

serializable_body <- function(x) {
  if (is.null(x)) return(x)
  if (vcr_configuration()$preserve_exact_body_bytes_for) {
    structure(base64enc::base64encode(charToRaw(x)), base64 = TRUE)
  } else {
    x
  }
}

body_from <- function(x) {
  # return hash_or_string unless hash_or_string.is_a?(Hash)
  # hash = hash_or_string
  if (attr(x, "base64")) {
    rawToChar(base64enc::base64decode(x))
  } else {
    try_encode_string(x, Encoding(x))
  }
}

try_encode_string <- function(string, encoding) {
  #return string if encoding.nil? || string.encoding.name == encoding
  # if (is.null(encoding) || ) return(string)

  # ASCII-8BIT just means binary, so encoding to it is nonsensical
  # and yet "\u00f6".encode("ASCII-8BIT") raises an error.
  # Instead, we'll force encode it (essentially just tagging it as binary)
  # return string.force_encoding(encoding) if encoding == "ASCII-8BIT"
  if (encoding == "ASCII-8BIT") return(string)
  return(string)

  # FIXME - Encoding() doesn't seem to fail with non-sensical
  # --- find something better
  #res <- tryCatch(Encoding(string) <- encoding, error = function(e) e)
  #string.encode(encoding)
  # rescue EncodingError => e
  #  struct_type = name.split('::').last.downcase
  #  warn "VCR: got `#{e.class.name}: #{e.message}` while trying to encode the #{string.encoding.name} " +
  #   "#{struct_type} body to the original body encoding (#{encoding}). Consider using the " +
  #   "`:preserve_exact_body_bytes` option to work around this."
  #  return(string)
}
