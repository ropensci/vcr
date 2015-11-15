#' Keeps track of the cassette serializers in a hash-like object
#' Note: LETS JUST USE YAML, EASY PEASY FOR NOW
#'
#' @examples
#' (aa <- serializers$new())
#' aa$name
#' aa$serializers
#' yaml_serializer <- aa$serializers$new()
#' yaml_serializer
serializers <- R6::R6Class("serializers",
  public = list(
    serializers = list(),
    name = "yaml",
    initialize = function(serializers = list(), name = "yaml") {
      self$serializers <- serializers
      self$name <- name
      private$serialize_get()
    }
  ),
  private = list(
    # Gets and sets a named serializer
    serialize_get = function() {
      if (!self$name %in% 'yaml') {
        stop(sprintf("The requested VCR cassette serializer (%s) is not registered.", self$name),
             call. = FALSE)
      }
      self$serializers <- switch(self$name,
        yaml = YAML
      )
    }
  )
)
