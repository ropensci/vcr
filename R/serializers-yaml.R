#' @title The YAML serializer
#' @description class with methods for serializing via the \pkg{yaml} package
#' @keywords internal
#' @examples \dontrun{
#' (yy <- YAML$new(path = "stuff2"))
#' fun <- yy$serialize()
#' fun(list(http_interactions = list(response = list(body = "bar"))),
#'   path = yy$path, bytes = FALSE)
#' yy$deserialize_path()
#' yy$deserialize_string(string = "- hey\n- hi\n- hello")
#' yy$deserialize_string(string = "- foo\n- bar\n- 3.14")
#' }
YAML <- R6::R6Class("YAML",
  public = list(
    #' @field file_extension (character) A file extension
    file_extension = ".yml",
    #' @field path (character) full path to the yaml file
    path = NULL,
    #' @field string (character) path string
    string = NULL,

    #' @description Create a new YAML object
    #' @param file_extension (character) A file extension
    #' @param path (character) full path to the yaml file
    #' @param string (character) path string
    #' @return A new `YAML` object
    initialize = function(file_extension = ".yml", path = NULL, string = NULL) {
      self$file_extension <- file_extension
      if (is.null(path)) {
        self$path <- paste0(cassette_path(), "/", basename(tempfile()), file_extension)
      } else {
        self$path <- paste0(cassette_path(), "/", path, file_extension)
      }
      self$string <- string
    },

    #' @description Serializes the given hash using internal fxn write_yaml
    #' @param x (list) the object to serialize
    #' @param path (character) the file path
    #' @param bytes (logical) whether to preserve exact body bytes or not
    #' @return (character) the YAML string to write to disk
    serialize = function(x, path, bytes) {
      #write_yaml(x, self$path)
      function(x, path, bytes) {
        write_yaml(x, path, bytes)
      }
    },

    #' @description Deserializes the given string using yaml::yaml.load
    #' @param string (character) the YAML string
    #' @return (list) the deserialized object, an R list
    deserialize_string = function(string = NULL) {
      if (is.null(self$string)) str <- string else self$string
      if (is.null(str)) stop("Must pass a string", call. = FALSE)
      yaml::yaml.load(str)
    },

    #' @description Deserializes the given string using yaml::yaml.load_file
    #' @return (list) the deserialized object, an R list
    deserialize_path = function() {
      # filter_sensitive_data replacement
      # FIXME: eventually move to higher level so that this happens
      #  regardless of serializer
      str <- sensitive_put_back(readLines(self$path))

      # to yaml
      tmp <- yaml::yaml.load(str)

      if (is.null(tmp)) {
        return(list())
      } else {
        # check for base64 encoding
        tmp$http_interactions <- lapply(tmp$http_interactions, function(z) {
          if (is_base64(z$response$body$string)) {
            resp_bod <- base64enc::base64decode(z$response$body$string)
            z$response$body$string <- if (is.raw(resp_bod)) resp_bod else rawToChar(resp_bod)
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
