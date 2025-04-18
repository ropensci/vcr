#' @title The request of an HTTPInteraction
#' @description object that handled all aspects of a request
#' @export
#' @keywords internal
#' @examples
#' url <- "https://hb.opencpu.org/post"
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
    #' @field method (character) http method
    method = NULL,
    #' @field uri (character) a uri
    uri = NULL,
    #' @field scheme (character) scheme (http or https)
    scheme = NULL,
    #' @field host (character) host (e.g., stuff.org)
    host = NULL,
    #' @field path (character) path (e.g., foo/bar)
    path = NULL,
    #' @field query (character) query params, named list
    query = NULL,
    #' @field body (character) named list
    body = NULL,
    #' @field headers (character) named list
    headers = NULL,
    #' @field skip_port_stripping (logical) whether to strip the port
    skip_port_stripping = FALSE,
    #' @field hash (character) a named list - internal use
    hash = NULL,
    #' @field opts (character) options - internal use
    opts = NULL,
    #' @field disk (logical) xx
    disk = NULL,
    #' @field fields (various) request body details
    fields = NULL,
    #' @field output (various) request output details, disk, memory, etc
    output = NULL,
    #' @field policies (various) http policies, used in httr2 only
    policies = NULL,

    #' @description Create a new `Request` object
    #' @param method (character) the HTTP method (i.e. head, options, get,
    #' post, put, patch or delete)
    #' @param uri (character) request URI
    #' @param body (character) request body
    #' @param headers (named list) request headers
    #' @param opts (named list) options internal use
    #' @param disk (boolean), is body a file on disk
    #' @param fields (various) post fields
    #' @param output (various) output details
    #' @param policies (various) http policies, used in httr2 only
    #' @param skip_port_stripping (logical) whether to strip the port.
    #' default: `FALSE`
    #' @return A new `Request` object
    initialize = function(
      method,
      uri,
      body,
      headers,
      opts,
      disk,
      fields,
      output,
      policies,
      skip_port_stripping = FALSE
    ) {
      if (!missing(method)) self$method <- tolower(method)
      if (!missing(body)) {
        if (inherits(body, "list")) {
          body <- paste(names(body), body, sep = "=", collapse = ",")
        }
        self$body <- body
      }
      if (!missing(headers)) self$headers <- headers
      if (!missing(uri)) {
        if (!skip_port_stripping) {
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
      if (!missing(disk)) self$disk <- disk
      if (!missing(fields)) self$fields <- fields
      if (!missing(output)) self$output <- output
      if (!missing(policies)) self$policies <- policies
    },

    #' @description Convert the request to a list
    #' @return list
    to_hash = function() {
      list(
        method = self$method,
        uri = self$uri,
        body = serializable_body(
          self$body,
          self$opts$preserve_exact_body_bytes %||% FALSE
        ),
        headers = self$headers,
        disk = self$disk
      )
    },

    #' @description Convert the request to a list
    #' @param hash a list
    #' @return a new `Request` object
    from_hash = function(hash) {
      Request$new(
        method = hash[['method']],
        uri = hash[['uri']],
        # body    = hash[['body']],
        body = body_from(hash[['body']]),
        headers = hash[['headers']],
        disk = hash[['disk']]
      )
    }
  ),
  private = list(
    without_standard_port = function(uri) {
      if (is.null(uri)) return(uri)
      u <- private$parsed_uri(uri)
      if (
        paste0(u$scheme, if (is.na(u$port)) NULL else u$port) %in%
          c('http', 'https/443')
      ) {
        return(uri)
      }
      u$port <- NA
      return(urltools::url_compose(u))
    },

    parsed_uri = function(uri) {
      urltools::url_parse(uri)
    }
  )
)

serializable_body <- function(body, preserve_exact_body_bytes = FALSE) {
  if (is.null(body)) {
    NULL
  } else if (preserve_exact_body_bytes) {
    structure(jsonlite::base64_enc(charToRaw(body)), base64 = TRUE)
  } else {
    body
  }
}

body_from <- function(x) {
  if (is.null(x)) x <- ""
  if (
    (!is.null(attr(x, "base64")) && attr(x, "base64"))
    # (!is.null(attr(x, "base64")) && attr(x, "base64")) || all(is_base64(x))
  ) {
    b64dec <- jsonlite::base64_dec(x)
    b64dec_r2c <- tryCatch(rawToChar(b64dec), error = function(e) e)
    if (inherits(b64dec_r2c, "error")) {
      # probably is binary (e.g., pdf), so can't be converted to char.
      b64dec
    } else {
      # probably was originally character data, so
      #  can convert to character from binary
      b64dec_r2c
    }
  } else {
    x
    # try_encode_string(x, Encoding_safe(x))
  }
}

is_base64 <- function(x, cassette) {
  if (!is.list(x)) {
    if ("base64" %in% names(attributes(x))) {
      return(attr(x, 'base64'))
    }
    return(FALSE)
  }

  # new base64 setup where it is stored in "base64_string"
  hasb64str <- "base64_string" %in% names(x)
  if (hasb64str) return(TRUE)

  if (cassette$preserve_exact_body_bytes && "string" %in% names(x)) {
    # old base64 setup where it was stored in "string"
    message("re-record cassettes using 'preserve_exact_body_bytes = TRUE'")
    return(TRUE)
  } else {
    # not using base64
    return(FALSE)
  }
}
