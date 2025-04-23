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
    #' @field body (character) named list
    body = NULL,
    #' @field headers (character) named list
    headers = NULL,
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
    #' @param disk (boolean), is body a file on disk
    #' @param fields (various) post fields
    #' @param output (various) output details
    #' @param policies (various) http policies, used in httr2 only
    #' @return A new `Request` object
    initialize = function(
      method,
      uri,
      body,
      headers,
      disk,
      fields,
      output,
      policies
    ) {
      if (!missing(method)) self$method <- tolower(method)
      if (!missing(body)) {
        if (inherits(body, "list")) {
          body <- paste(names(body), body, sep = "=", collapse = ",")
        }
        self$body <- body
      }
      if (!missing(headers)) self$headers <- headers
      if (!missing(uri)) self$uri <- uri
      if (!missing(disk)) self$disk <- disk
      if (!missing(fields)) self$fields <- fields
      if (!missing(output)) self$output <- output
      if (!missing(policies)) self$policies <- policies
    }
  )
)
