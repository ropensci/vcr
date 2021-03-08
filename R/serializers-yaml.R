# yy <- YAML$new(path = "stuff2")
# yy
# yy$file_extension
# fun <- yy$serialize()
# fun(list(http_interactions = list(response = list(body = "bar"))),
#   path = yy$path, bytes = FALSE)
# yy$deserialize()
# }

#' @title The YAML serializer
#' @description class with methods for serializing via the \pkg{yaml} package
#' @keywords internal
YAML <- R6::R6Class("YAML",
  inherit = Serializer,
  public = list(
    #' @description Create a new YAML object
    #' @param path (character) path to the cassette, excluding the cassette
    #' directory and the file extension
    #' @return A new `YAML` object
    initialize = function(path = NULL) {
      super$initialize(".yml", path)
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

    #' @description Deserializes the content at the path using
    #' yaml::yaml.load_file
    #' @return (list) the deserialized object, an R list
    deserialize = function() {
      tmp <- yaml_load_desecret(self$path)
      private$process_body(tmp)
    }
  )
)

yaml_load_desecret <- function(path) {
  str <- sensitive_put_back(readLines(path, encoding = "UTF-8"))
  tmp <- yaml::yaml.load(str)
  tmp <- query_params_put_back(tmp)
  tmp
}
