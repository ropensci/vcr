#' @title A response object
#' @description Custom vcr http response object
#' @export
#' @keywords internal
VcrResponse <- R6::R6Class(
  "VcrResponse",
  public = list(
    #' @field status the status of the response
    status = NULL,
    #' @field headers the response headers
    headers = NULL,
    #' @field body the response body
    body = NULL,
    #' @field disk a boolean
    disk = NULL,

    #' @description Create a new VcrResponse object
    #' @param status the status of the response
    #' @param headers the response headers
    #' @param body the response body
    #' @param disk boolean, is body a file on disk
    #' @return A new `VcrResponse` object
    initialize = function(
      status,
      headers,
      body,
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
      if (!missing(disk)) self$disk <- disk
    },

    #' @description print method for the `VcrResponse` class
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) cat("<VcrResponse> ", sep = "\n")
  )
)
