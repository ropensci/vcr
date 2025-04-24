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

decode_interactions <- function(x, preserve_bytes = FALSE) {
  if (is.null(x)) return(list())

  x$http_interactions <- lapply(x$http_interactions, function(z) {
    z$request$body <- decode_body(z$request$body)
    z$response$body <- decode_body(z$response$body, preserve_bytes)
    z
  })

  x
}

serializer_fetch <- function(type, path, name, preserve_bytes = FALSE) {
  switch(
    type,
    json = JSON$new(path, name, preserve_bytes = preserve_bytes),
    yaml = YAML$new(path, name, preserve_bytes = preserve_bytes),
    cli::cli_abort("Unsupported cassette serializer {.str {type}}.")
  )
}
