#' The request of an HTTPInteraction
#'
#' @export
#' @keywords internal
#' @param method the HTTP method (i.e. :head, :options, :get, :post, :put,
#' :patch or :delete)
#' @param uri the request URI
#' @param body the request body
#' @param headers the request headers
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{to_hash()}}{
#'       Get a hash from the class itself
#'     }
#'     \item{\code{from_hash()}}{
#'       Create a `Request` class object from a hash
#'     }
#'   }
#' @examples
#' url <- "https://eu.httpbin.org/post"
#' body <- list(foo = "bar")
#' headers <- list(
#'   `User-Agent` = "libcurl/7.54.0 r-curl/3.2 crul/0.5.2",
#'   `Accept-Encoding` = "gzip, deflate",
#'   Accept = "application/json, text/xml, application/xml, */*"
#' )
#'
#' (x <- Request$new("POST", url, body, headers))
#' x$body
#' x$method
#' x$uri
#' x$host
#' x$path
#' x$headers
#' h <- x$to_hash()
#' x$from_hash(h)
Request <- R6::R6Class(
  'Request',
   public = list(
     method = NULL,
     uri = NULL,
     scheme = NULL,
     host = NULL,
     path = NULL,
     query = NULL,
     body = NULL,
     headers = NULL,
     skip_port_stripping = FALSE,
     hash = NULL,
     opts = NULL,

     initialize = function(method, uri, body, headers, opts) {
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
         # parse URI to get host and path
         tmp <- eval(parse(text = vcr_c$uri_parser))(self$uri)
         self$scheme <- tmp$scheme
         self$host <- tmp$domain
         self$path <- tmp$path
         self$query <- tmp$parameter
       }
       if (!missing(opts)) self$opts <- opts
     },

     to_hash = function() {
       self$hash <- list(
         method  = self$method,
         uri     = self$uri,
         body    = serializable_body(self$body, self$opts$preserve_exact_body_bytes %||% FALSE),
         headers = self$headers
       )
       return(self$hash)
     },

     from_hash = function(hash) {
       Request$new(
         method  = hash[['method']],
         uri     = hash[['uri']],
         body    = body_from(hash[['body']]),
         headers = hash[['headers']]
       )
     }
   ),
   private = list(
     without_standard_port = function(uri) {
       if (is.null(uri)) return(uri)
       u <- private$parsed_uri(uri)
       if (paste0(u$scheme, if (is.na(u$port)) NULL else u$port) %in% c('http', 'https/443')) {
         return(uri)
       }
       u$port <- NA
       return(urltools::url_compose(u))
     },

     parsed_uri = function(uri) {
       #eval(parse(text = vcr_configuration()$uri_parser))(uri)
       urltools::url_parse(uri)
     }

     # make_uri = function(x) {
     #   paste0("%s://%s", x$scheme, file.path(x$domain, x$path), )
     # }
   )
)

serializable_body <- function(x, preserve_exact_body_bytes = FALSE) {
  if (is.null(x)) return(x)
  # if (vcr_configuration()$preserve_exact_body_bytes) {
  if (preserve_exact_body_bytes) {
    structure(base64enc::base64encode(charToRaw(x)), base64 = TRUE)
  } else {
    x
  }
}

body_from <- function(x) {
  # return hash_or_string unless hash_or_string.is_a?(Hash)
  # hash = hash_or_string
  if (is.null(x)) x <- ""
  if (is.null(attr(x, "base64"))) return(try_encode_string(x, Encoding(x)))
  if (attr(x, "base64") || is_base64(x)) {
    rawToChar(base64enc::base64decode(x))
  } else {
    try_encode_string(x, Encoding(x))
  }
}

is_base64 <- function(x) {
  grepl(b64_pattern, x)
}

b64_pattern <- "^(?:[A-Za-z0-9+/]{4})*(?:[A-Za-z0-9+/]{2}==|[A-Za-z0-9+/]{3}=|[A-Za-z0-9+/]{4})$"

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
