JSON <- R6::R6Class(
  "JSON",
  inherit = Serializer,
  public = list(
    initialize = function(path, name, preserve_bytes = FALSE) {
      super$initialize(path, name, ".json", preserve_bytes = preserve_bytes)
    },

    serialize = function(data) {
      out <- encode_interactions(data, self$preserve_bytes)
      jsonlite::write_json(
        out,
        self$path,
        auto_unbox = TRUE,
        pretty = vcr_c$json_pretty
      )
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
