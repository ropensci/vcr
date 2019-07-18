#' RequestMatcherRegistry class
#'
#' @export
#' @param name matcher name
#' @param func function that describes a matcher, should return
#' a single boolean
#' @param r1,r2 two [Request] class objects
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
          sprintf("There is already a vcr request matcher registered for %s. Overriding it.",
                  name)
        )
      }
      self$registry[[name]] <- Matcher$new(func = func)
    },

    register_built_ins = function() {
      self$register("method", function(r1, r2) r1$method == r2$method)
      self$register("uri", function(r1, r2) r1$uri == r2$uri)
      self$register("body", function(r1, r2) identical(r1$body, r2$body))
      self$register('headers', function(r1, r2) identical(r1$headers, r2$headers))
      self$register("host", function(r1, r2) identical(r1$host, r2$host))
      self$register("path", function(r1, r2) identical(r1$path, r2$path))
      self$register("query", function(r1, r2) identical(r1$query, r2$query))
      self$try_to_register_body_as_json()
    },

    try_to_register_body_as_json = function(r1, r2) {
      if (!requireNamespace("jsonlite", quietly = TRUE)) {
        stop("please install jsonlite", call. = FALSE)
      }

      self$register("body_as_json", function(r1, r2) {
        tc <- tryCatch(
          identical(jsonlite::fromJSON(r1$body), jsonlite::fromJSON(r2$body)),
          error = function(e) e
        )
        if (inherits(tc, "error")) FALSE else tc
      })
    }
  )
)

# generate matcher functions
Matcher <- R6::R6Class(
  'Matcher',
  public = list(
    func = NULL,
    initialize = function(func) self$func <- func,
    matches = function(...) self$func(...)
  )
)
