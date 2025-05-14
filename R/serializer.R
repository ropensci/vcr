serializer_fetch <- function(type, path, name, ...) {
  switch(
    type,
    json = JSON$new(path, name, ...),
    yaml = YAML$new(path, name, ...),
    qs2 = QS2$new(path, name, ...),
    cli::cli_abort("Unsupported cassette serializer {.str {type}}.")
  )
}

Serializer <- R6::R6Class(
  "Serializer",
  public = list(
    file_extension = NULL,
    path = NULL,
    preserve_bytes = FALSE,
    matchers = character(),

    initialize = function(
      path,
      name,
      ext,
      preserve_bytes = FALSE,
      matchers = c("method", "uri")
    ) {
      self$file_extension <- ext
      self$path <- paste0(path, "/", name, self$file_extension)
      self$preserve_bytes = preserve_bytes
      self$matchers <- matchers
    },
    serialize = function(data) NULL,
    deserialize = function() NULL
  )
)

JSON <- R6::R6Class(
  "JSON",
  inherit = Serializer,
  public = list(
    initialize = function(
      path,
      name,
      preserve_bytes = FALSE,
      matchers = c("method", "uri")
    ) {
      super$initialize(
        path,
        name,
        ".json",
        preserve_bytes = preserve_bytes,
        matchers = matchers
      )
    },

    serialize = function(data) {
      out <- encode_interactions(
        data,
        preserve_bytes = self$preserve_bytes,
        matchers = self$matchers
      )
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
    initialize = function(
      path,
      name,
      preserve_bytes = FALSE,
      matchers = c("method", "uri")
    ) {
      super$initialize(
        path,
        name,
        ".yml",
        preserve_bytes = preserve_bytes,
        matchers = matchers
      )
    },

    serialize = function(data) {
      out <- encode_interactions(
        data,
        preserve_bytes = self$preserve_bytes,
        matchers = self$matchers
      )
      yaml::write_yaml(out, self$path)
    },

    deserialize = function() {
      input <- yaml::read_yaml(self$path)
      decode_interactions(input, self$preserve_bytes)
    }
  )
)

QS2 <- R6::R6Class(
  "QS2",
  inherit = Serializer,
  public = list(
    initialize = function(
      path,
      name,
      preserve_bytes = FALSE,
      matchers = c("method", "uri")
    ) {
      check_installed("qs2")
      super$initialize(
        path,
        name,
        ".qs2",
        preserve_bytes = preserve_bytes,
        matchers = matchers
      )
    },

    serialize = function(data) {
      out <- encode_interactions(
        data,
        preserve_bytes = self$preserve_bytes,
        matchers = self$matchers
      )
      qs2::qs_save(object = out, file = self$path)
    },

    deserialize = function() {
      input <- qs2::qs_read(file = self$path)
      decode_interactions(input, self$preserve_bytes)
    }
  )
)
