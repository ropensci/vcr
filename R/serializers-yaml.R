# yy <- YAML$new(path = "stuff2")
# yy
# yy$file_extension
# fun <- yy$serialize()
# fun(list(http_interactions = list(response = list(body = "bar"))),
#   path = yy$path, bytes = FALSE)
# yy$deserialize()
# }

YAML <- R6::R6Class(
  "YAML",
  inherit = Serializer,
  public = list(
    initialize = function(path, name) {
      super$initialize(path, name, ".yml")
    },

    serialize = function(data, preserve_bytes) {
      write_yaml(data, self$path, preserve_bytes)
    },

    deserialize = function(cassette) {
      tmp <- yaml_load_desecret(self$path)
      private$process_body(tmp, cassette)
    }
  )
)

yaml_load_desecret <- function(path) {
  str <- sensitive_put_back(readLines(path, encoding = "UTF-8"))
  tmp <- yaml::yaml.load(str)
  tmp <- query_params_put_back(tmp)
  tmp
}
