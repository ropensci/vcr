#' Keeps track of the cassette serializers in a hash-like object
#'
#' @export
#' @keywords internal
#' @param serializers List of serializers
#' @param name Name of a serializer. Only option is "yaml" for now
#' @details
#' \strong{Private Methods}
#'   \describe{
#'     \item{\code{serialize_get()}}{
#'       Gets a named serializer. This is also run on initialize of this function
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' (aa <- Serializers$new())
#' aa$name
#' aa$serializers
#' yaml_serializer <- aa$serializers$new()
#' yaml_serializer
#' }
Serializers <- R6::R6Class(
  "Serializers",
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

#' @export
#' @rdname Serializers
serializer_fetch <- function(x = "yaml", name) {
  ser <- Serializers$new(name = x)
  ser <- ser$serializers
  ser$new(path = name)
}
