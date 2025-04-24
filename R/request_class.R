#' @title A request object
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
    #' @param skip_port_stripping (logical) whether to strip the port.
    #' default: `FALSE`
    #' @return A new `Request` object
    initialize = function(
      method,
      uri,
      body,
      headers,
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
