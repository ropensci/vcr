#' RequestMatcherRegistry class
#'
#' @export
#' @param name matcher name
#' @param func function that describes a matcher, should return a boolean
#' @param r1,r2 two requests, each of class `HTTPInteraction`
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{register(name, func)}}{
#'       Register a custom matcher.
#'     }
#'     \item{\code{register_built_ins()}}{
#'       Register all built in matchers.
#'     }
#'     \item{\code{try_to_register_body_as_json(r1, r2)}}{
#'       Try to register body as JSON.
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (x <- RequestMatcherRegistry$new())
#' x$default_matchers
#' x$registry
#' }
RequestMatcherRegistry <- R6::R6Class(
  'RequestMatcherRegistry',
  public = list(
    registry = NULL,
    default_matchers = NULL,

    initialize = function(registry = list(),
                          default_matchers = list('method', 'uri')) {
      self$registry <- registry
      self$default_matchers <- default_matchers
      self$register_built_ins()
    },

    register = function(name, func) {
      if (name %in% self$registry) {
        warning(
          sprintf("There is already a VCR request matcher registered for %s. Overriding it.", name)
          )
      }

      self$registry[[name]] <- Matcher$new(func = func)
    },

    register_built_ins = function() {
      self$register("method", function(r1, r2) r1$method == r2$method)

      self$register("uri", function(r1, r2) r1$uri == r2$uri)

      self$register("body", function(r1, r2) r1$body == r2$body)

      self$register('headers', function(r1, r2) r1$headers == r2$headers)

      self$register("host", function(r1, r2) {
        r1$parsed_uri$host == r2$parsed_uri$host
      })

      self$register("path", function(r1, r2) {
        r1$parsed_uri$path == r2$parsed_uri$path
      })

      self$register("query", function(r1, r2) {
        vcr_configuration()$query_parser$call(r1$parsed_uri$query) ==
          vcr_configuration()$query_parser$call(r2$parsed_uri$query)
      })

      self$try_to_register_body_as_json()
    },

    try_to_register_body_as_json = function(r1, r2) {
      if (!requireNamespace("jsonlite")) {
        stop("please install jsonlite", call. = FALSE)
      }

      self$register("body_as_json", function(r1, r2) {
        tryCatch(jsonlite::fromJSON(r1$body) == jsonlite::fromJSON(r2$body),
                 error = function(e) e)
      })
    }
  )
)

# closure to generate matcher functions
Matcher <- R6::R6Class(
  'Matcher',
  public = list(
    func = NULL,

    initialize = function(func) {
      self$func <- func
    },

    matches = function(...) {
      self$func(...)
    }
    # matches = function(request1, request2) {
    #   self$func(request1, request2)
    # }
  )
)
