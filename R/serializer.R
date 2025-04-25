serializer_fetch <- function(type, path, name, preserve_bytes = FALSE) {
  switch(
    type,
    json = JSON$new(path, name, preserve_bytes = preserve_bytes),
    yaml = YAML$new(path, name, preserve_bytes = preserve_bytes),
    compressed = Compressed$new(path, name, preserve_bytes = preserve_bytes),
    cli::cli_abort("Unsupported cassette serializer {.str {type}}.")
  )
}

Serializer <- R6::R6Class(
  "Serializer",
  public = list(
    file_extension = NULL,
    path = NULL,
    preserve_bytes = FALSE,

    initialize = function(path, name, ext, preserve_bytes = FALSE) {
      self$file_extension <- ext
      self$path <- paste0(path, "/", name, self$file_extension)
      self$preserve_bytes = preserve_bytes
    },
    serialize = function(data) NULL,
    deserialize = function() NULL
  )
)

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

Compressed <- R6::R6Class(
  "Compressed",
  inherit = Serializer,
  public = list(
    initialize = function(path, name, preserve_bytes = FALSE) {
      check_installed("qs2")
      super$initialize(path, name, ".qs2", preserve_bytes = preserve_bytes)
    },

    serialize = function(data) {
      out <- encode_interactions(data, self$preserve_bytes)
      json <- as.character(jsonlite::toJSON(out, auto_unbox = TRUE))
      qs2::qs_save(object = json, file = self$path)
    },

    deserialize = function() {
      str <- qs2::qs_read(file = self$path)
      str <- sensitive_put_back(str)
      interactions <- jsonlite::fromJSON(str, FALSE)
      interactions <- query_params_put_back(interactions)
      interactions <- decode_interactions(interactions, self$preserve_bytes)
      interactions
    }
  )
)
