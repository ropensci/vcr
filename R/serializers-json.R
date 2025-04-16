# ww <- JSON$new(path = "stuff3")
# ww
# ww$file_extension
# fun <- ww$serialize()
# fun(list(http_interactions = list(response = list(body = "bar"))),
#   path = ww$path, bytes = FALSE)
# ww$deserialize()

JSON <- R6::R6Class(
  "JSON",
  inherit = Serializer,
  public = list(
    initialize = function(path, name) {
      super$initialize(path, name, ".json")
    },

    serialize = function(data, preserve_bytes) {
      write_json(data, self$path, preserve_bytes)
    },

    deserialize = function(cassette) {
      str <- sensitive_put_back(readLines(self$path))
      tmp <- jsonlite::fromJSON(str, FALSE)
      tmp <- query_params_put_back(tmp)
      private$process_body(tmp, cassette)
    }
  )
)
