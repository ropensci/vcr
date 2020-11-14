#' @title The YAML serializer
#' @description class with methods for serializing via the \pkg{yaml} package
#' @keywords internal
#' @examples \dontrun{
#' yy <- YAML$new(path = "stuff2")
#' yy
#' yy$file_extension
#' fun <- yy$serialize()
#' fun(list(http_interactions = list(response = list(body = "bar"))),
#'   path = yy$path, bytes = FALSE)
#' yy$deserialize_path()
#' yy$deserialize_string(string = "- hey\n- hi\n- hello")
#' yy$deserialize_string(string = "- foo\n- bar\n- 3.14")
#' }
YAML <- R6::R6Class("YAML",
  inherit = Serializer,
  public = list(
    #' @description Create a new YAML object
    #' @param path (character) path to the cassette, excluding the cassette
    #' directory and the file extension. only use if not passing a string
    #' @param string (character) path string. only use if not passing a path
    #' @return A new `YAML` object
    initialize = function(path = NULL, string = NULL) {
      super$initialize(".yml", path, string)
    },

    #' @description Serializes the given hash using internal fxn write_yaml
    #' @param x (list) the object to serialize
    #' @param path (character) the file path
    #' @param bytes (logical) whether to preserve exact body bytes or not
    #' @return (character) the YAML string to write to disk
    serialize = function(x, path, bytes) {
      #write_yaml(x, self$path)
      function(x, path, bytes) {
        write_yaml(x, path, bytes)
      }
    },

    #' @description Deserializes the given string using yaml::yaml.load
    #' @param string (character) the YAML string
    #' @return (list) the deserialized object, an R list
    deserialize_string = function(string = NULL) {
      str <- if (is.null(self$string)) string else self$string
      if (is.null(str)) stop("Must pass a string", call. = FALSE)
      yaml::yaml.load(str)
    },

    #' @description Deserializes the given string using yaml::yaml.load_file
    #' @return (list) the deserialized object, an R list
    deserialize_path = function() {
      str <- sensitive_put_back(readLines(self$path))
      tmp <- yaml::yaml.load(str)
      private$process_body(tmp)
    }
  )
)
