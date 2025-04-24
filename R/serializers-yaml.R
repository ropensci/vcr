YAML <- R6::R6Class(
  "YAML",
  inherit = Serializer,
  public = list(
    initialize = function(path, name, preserve_bytes = FALSE) {
      super$initialize(path, name, ".yml", preserve_bytes = preserve_bytes)
    },

    serialize = function(data) {
      out <- encode_interactions(data, self$preserve_bytes)
      yaml::write_yaml(out, self$path)
    },

    deserialize = function() {
      str <- sensitive_put_back(readLines(self$path, encoding = "UTF-8"))
      interactions <- yaml::yaml.load(str)
      interactions <- query_params_put_back(interactions)
      interactions <- decode_interactions(interactions, self$preserve_bytes)
      interactions
    }
  )
)
