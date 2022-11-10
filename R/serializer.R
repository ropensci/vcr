#' Serializer class - base class for JSON/YAML serializers
#' @keywords internal
Serializer <- R6::R6Class("Serializer",
  public = list(
    #' @field file_extension (character) A file extension
    file_extension = NULL,
    #' @field path (character) full path to the yaml file
    path = NULL,

    #' @description Create a new YAML object
    #' @param file_extension (character) A file extension
    #' @param path (character) path to the cassette, excluding the cassette
    #' directory and the file extension
    #' @return A new `YAML` object
    initialize = function(file_extension = NULL, path = NULL) {
      self$file_extension <- file_extension
      if (is.null(path)) {
        self$path <- paste0(cassette_path(), "/", basename(tempfile()), self$file_extension)
      } else {
        self$path <- paste0(cassette_path(), "/", path, self$file_extension)
      }
    },
    #' @description Serializes a hash - REPLACED BY YAML/JSON METHODS
    #' @param x (list) the object to serialize
    #' @param path (character) the file path
    #' @param bytes (logical) whether to preserve exact body bytes or not
    #' @return (character) the YAML or JSON string to write to disk
    serialize = function(x, path, bytes) {},
    #' @description Serializes a file - REPLACED BY YAML/JSON METHODS
    deserialize = function() {}
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
            z$response$body[[slot]] <- private$strip_newlines(z$response$body[[slot]])
            b64dec <- base64enc::base64decode(z$response$body[[slot]])
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
