serializer_fetch <- function(type, path, name, preserve_bytes = FALSE) {
  switch(
    type,
    json = JSON$new(path, name, preserve_bytes = preserve_bytes),
    yaml = YAML$new(path, name, preserve_bytes = preserve_bytes),
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
        pretty = the$config$json_pretty
      )
    },

    deserialize = function() {
      input <- jsonlite::read_json(self$path)
      decode_interactions(input, self$preserve_bytes)
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
      input <- yaml::read_yaml(self$path)
      decode_interactions(input, self$preserve_bytes)
    }
  )
)
