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
    strip_newlines = function(x) {
      if (!inherits(x, "character")) return(x)
      gsub("[\r\n]", "", x)
    },
    process_body = function(x, cassette) {
      if (is.null(x)) {
        return(list())
      } else {
        # check for base64 encoding
        x$http_interactions <- lapply(x$http_interactions, function(z) {
          if (is_base64(z$response$body, cassette)) {
            # string or base64_string?
            str_slots <- c('string', 'base64_string')
            slot <- str_slots[str_slots %in% names(z$response$body)]

            # if character and newlines detected, remove newlines
            z$response$body[[slot]] <- private$strip_newlines(z$response$body[[
              slot
            ]])
            b64dec <- jsonlite::base64_dec(z$response$body[[slot]])
            b64dec_r2c <- tryCatch(rawToChar(b64dec), error = function(e) e)
            z$response$body[[slot]] <- if (inherits(b64dec_r2c, "error")) {
              # probably is binary (e.g., pdf), so can't be converted to char.
              b64dec
            } else {
              # probably was originally character data, so
              #  can convert to character from binary
              b64dec_r2c
            }

            # set encoding to empty string, we're ignoring it for now
            z$response$body$encoding <- ""
          }
          return(z)
        })
        return(x)
      }
    }
  )
)

serializer_fetch <- function(type, path, name) {
  switch(
    type,
    json = JSON$new(path, name),
    yaml = YAML$new(path, name),
    cli::cli_abort("Unsupported cassette serializer {.str {type}}.")
  )
}
