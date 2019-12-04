#' @title Cassette serializers
#' @description Keeps track of the cassette serializers in a hash-like object
#' @export
#' @keywords internal
#' @details
#' \strong{Private Methods}
#'   \describe{
#'     \item{\code{serialize_get()}}{
#'       Gets a named serializer. This is also run on initialize of this function
#'     }
#'   }
#' @examples \dontrun{
#' (aa <- Serializers$new())
#' aa$name
#' aa$serializers
#' yaml_serializer <- aa$serializers$new()
#' yaml_serializer
#' 
#' x <- Serializers$new(name = "json")
#' x$serializers$new()
#' }
Serializers <- R6::R6Class(
  "Serializers",
  public = list(
    #' @field serializers (list) list of serializer names
    serializers = list(),
    #' @field name (character) Name of a serializer. "yaml" or "json"
    name = "yaml",

    #' @description Create a new Serializers object
    #' @param serializers (list) list of serializer names
    #' @param name (character) Name of a serializer. "yaml" or "json"
    #' @return A new `Serializers` object
    initialize = function(serializers = list(), name = "yaml") {
      self$serializers <- serializers
      self$name <- name
      private$serialize_get()
    }
  ),

  private = list(
    serialize_get = function() {
      if (!self$name %in% c("yaml", "json")) {
        stop(sprintf("The requested vcr cassette serializer (%s) is not registered.", self$name),
             call. = FALSE)
      }
      self$serializers <- switch(self$name,
        yaml = YAML,
        json = JSON
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
