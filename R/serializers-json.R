JSON <- R6::R6Class(
  "JSON",
  inherit = Serializer,
  public = list(
    initialize = function(path, name, preserve_bytes = FALSE) {
      super$initialize(path, name, ".json", preserve_bytes = preserve_bytes)
    },

    serialize = function(data) {
      write_json(data, self$path, self$preserve_bytes)
    },

    deserialize = function() {
      str <- sensitive_put_back(readLines(self$path))
      interactions <- jsonlite::fromJSON(str, FALSE)
      interactions <- query_params_put_back(interactions)
      interactions <- decode_interactions(interactions, self$preserve_bytes)
      interactions
    }
  )
)
