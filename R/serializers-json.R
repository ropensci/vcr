#' @title The JSON serializer
#' @description class with methods for serializing via \pkg{jsonlite}
#' @keywords internal
#' @examples \dontrun{
#' ww <- JSON$new(path = "stuff3")
#' ww
#' ww$file_extension
#' fun <- ww$serialize()
#' fun(list(http_interactions = list(response = list(body = "bar"))),
#'   path = ww$path, bytes = FALSE)
#' ww$deserialize_path()
#' ww$deserialize_string(string = '["hey", "hi", "hello"]')
#' ww$deserialize_string(string = '{"foo": "bar"}')
#' }
JSON <- R6::R6Class("JSON",
  inherit = Serializer,
  public = list(
    #' @description Create a new `JSON` object
    #' @param path (character) full path to the yaml file
    #' @param string (character) path string
    #' @return A new `JSON` object
    initialize = function(path = NULL, string = NULL) {
      super$initialize(".json", path, string)
    },

    #' @description Serializes the given hash using internal fxn write_json
    #' @param x (list) the object to serialize
    #' @param path (character) the file path
    #' @param bytes (logical) whether to preserve exact body bytes or not
    #' @return (character) the json string to write to disk
    serialize = function(x, path, bytes) {
      function(x, path, bytes) {
        write_json(x, path, bytes)
      }
    },

    #' @description Deserializes the given string using jsonlite::fromJSON
    #' @param string (character) the YAML string
    #' @return (list) the deserialized object, an R list
    deserialize_string = function(string = NULL) {
      if (is.null(self$string)) str <- string else self$string
      if (is.null(str)) stop("Must pass a string", call. = FALSE)
      jsonlite::fromJSON(str)
    },

    #' @description Deserializes the given string using jsonlite::fromJSON
    #' @return (list) the deserialized object, an R list
    deserialize_path = function() {
      str <- sensitive_put_back(readLines(self$path))
      tmp <- jsonlite::fromJSON(str, FALSE)
      private$process_body(tmp)
    }
  )
)
