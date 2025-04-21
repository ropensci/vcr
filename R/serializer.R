Serializer <- R6::R6Class(
  "Serializer",
  public = list(
    file_extension = NULL,
    path = NULL,

    initialize = function(path, name, ext) {
      self$file_extension <- ext
      self$path <- paste0(path, "/", name, self$file_extension)
    },
    serialize = function(data, preserve_bytes = TRUE) {
    },
    deserialize = function() {
    }
  ),
  private = list(
    process_body = function(x, cassette) {
      if (is.null(x)) {
        return(list())
      }

      x$http_interactions <- lapply(x$http_interactions, function(z) {
        z$request$body <- decode_body(z$request$body)
        z$response$body <- decode_body(
          z$response$body,
          cassette$preserve_exact_body_bytes
        )
        return(z)
      })
      return(x)
    }
  )
)

decode_body <- function(body, preserve_bytes = FALSE) {
  if (has_name(body, "string") && preserve_bytes) {
    warning("re-record cassettes using 'preserve_exact_body_bytes = TRUE'")
  } else if (has_name(body, "base64_string")) {
    body$base64_string <- from_base64(body$base64_string)
  }
  body
}

serializer_fetch <- function(type, path, name) {
  switch(
    type,
    json = JSON$new(path, name),
    yaml = YAML$new(path, name),
    cli::cli_abort("Unsupported cassette serializer {.str {type}}.")
  )
}
