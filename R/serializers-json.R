#' @title The JSON serializer
#' @description class with methods for serializing via the \pkg{jsonlite} package
#' @keywords internal
#' @examples \dontrun{
#' (yy <- JSON$new(path = "stuff2"))
#' fun <- yy$serialize()
#' fun(list(http_interactions = list(response = list(body = "bar"))),
#'   path = yy$path, bytes = FALSE)
#' yy$deserialize_path()
#' yy$deserialize_string(string = "- hey\n- hi\n- hello")
#' yy$deserialize_string(string = "- foo\n- bar\n- 3.14")
#' }
JSON <- R6::R6Class("JSON",
  public = list(
    #' @field file_extension (character) A file extension
    file_extension = ".json",
    #' @field path (character) full path to the json file
    path = NULL,
    #' @field string (character) path string
    string = NULL,

    #' @description Create a new `JSON` object
    #' @param file_extension (character) A file extension
    #' @param path (character) full path to the yaml file
    #' @param string (character) path string
    #' @return A new `JSON` object
    initialize = function(file_extension = ".json", path = NULL, string = NULL) {
      self$file_extension <- file_extension
      if (is.null(path)) {
        self$path <- paste0(cassette_path(), "/", basename(tempfile()), file_extension)
      } else {
        self$path <- paste0(cassette_path(), "/", path, file_extension)
      }
      self$string <- string
    },

    #' @description Serializes the given hash using internal fxn write_json
    #' @param x (list) the object to serialize
    #' @param path (character) the file path
    #' @param bytes (logical) whether to preserve exact body bytes or not
    #' @return (character) the YAML string to write to disk
    serialize = function(x, path, bytes) {
      function(x, path, bytes) {
        write_json(x, path, bytes)
      }
    },

    #' @description Deserializes the given string using jsonlite::fromJSON
    #' @param string (character) the YAML string
    #' @return (list) the deserialized object, an R list
    deserialize_string = function(string = NULL) {
      if (is.null(self$string)) str <- string else self$string
      if (is.null(str)) stop("Must pass a string", call. = FALSE)
      jsonlite::fromJSON(str)
    },

    #' @description Deserializes the given string using jsonlite::fromJSON
    #' @return (list) the deserialized object, an R list
    deserialize_path = function() {
      # filter_sensitive_data replacement
      # FIXME: eventually move to higher level so that this happens
      #  regardless of serializer
      str <- sensitive_put_back(readLines(self$path))

      # to json
      tmp <- jsonlite::fromJSON(str, FALSE)

      if (is.null(tmp)) {
        return(list())
      } else {
        # check for base64 encoding
        tmp$http_interactions <- lapply(tmp$http_interactions, function(z) {
          if (is_base64(z$response$body$string)) {
            b64dec <- base64enc::base64decode(z$response$body$string)
            b64dec_r2c <- tryCatch(rawToChar(b64dec), error = function(e) e)
            z$response$body$string <- if (inherits(b64dec_r2c, "error")) {
              # probably is binary (e.g., pdf), so can't be converted to char.
              b64dec
            } else {
              # probably was originally character data, so 
              #  can convert to character from binary
              b64dec_r2c
            }
            z$response$body$encoding <-
              suppressMessages(encoding_guess(z$response$body$string, TRUE))
          }
          return(z)
        })
        return(tmp)
      }
    }
  )
)
