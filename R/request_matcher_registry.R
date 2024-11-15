#' @title RequestMatcherRegistry
#' @description handles request matchers
#' @export
#' @examples \dontrun{
#' (x <- RequestMatcherRegistry$new())
#' x$default_matchers
#' x$registry
#' }
RequestMatcherRegistry <- R6::R6Class(
  'RequestMatcherRegistry',
  public = list(
    #' @field registry initialze registry list with a request, or leave empty
    registry = NULL,
    #' @field default_matchers request matchers to use. default: method, uri
    default_matchers = NULL,

    #' @description Create a new RequestMatcherRegistry object
    #' @param registry initialze registry list with a request, or leave empty
    #' @param default_matchers request matchers to use. default: method, uri
    #' @return A new `RequestMatcherRegistry` object
    initialize = function(registry = list(),
                          default_matchers = list('method', 'uri')) {
      self$registry <- registry
      self$default_matchers <- default_matchers
      self$register_built_ins()
    },

    #' @description Register a custom matcher
    #' @param name matcher name
    #' @param func function that describes a matcher, should return
    #' a single boolean
    #' @return no return; registers the matcher
    register = function(name, func) {
      if (name %in% self$registry) {
        warning(
          sprintf("There is already a vcr request matcher registered for %s. Overriding it.",
                  name)
        )
      }
      self$registry[[name]] <- Matcher$new(func = func)
    },

    #' @description Register all built in matchers
    #' @return no return; registers all built in matchers
    #' @note r1=from new request; r2=from recorded interaction
    register_built_ins = function() {
      self$register("method", function(r1, r2) r1$method == r2$method)
      self$register("uri", function(r1, r2)
              identical(
                curl::curl_unescape(query_params_remove_str(r1$uri)), curl::curl_unescape(r2$uri))
              )
      self$register("body", function(r1, r2) identical(r1$body, r2$body))
      self$register('headers', function(r1, r2) identical(r1$headers, r2$headers))
      self$register("host", function(r1, r2) identical(r1$host, r2$host))
      self$register("path", function(r1, r2)
        identical(sub("/$", "", r1$path), sub("/$", "", r2$path)))
      self$register("query", function(r1, r2)
        identical(curl::curl_unescape(r1$query), curl::curl_unescape(r2$query)))
      self$try_to_register_body_as_json()
    },

    #' @description Try to register body as JSON
    #' @param r1,r2 [Request] class objects
    #' @return no return; registers the matcher
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
