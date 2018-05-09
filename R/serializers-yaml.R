#' The YAML serializer
#'
#' @keywords internal
#' @param file_extension (character) A file extension
#' @param path (character) full path to the yaml file
#' @param string (character) path string
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{serialize(x)}}{
#'       Serializes the given hash using internal fxn write_yaml
#'     }
#'     \item{\code{deserialize_string(string = NULL)}}{
#'       Deserializes the given string using yaml::yaml.load
#'     }
#'     \item{\code{deserialize_path()}}{
#'       Deserializes the given string using yaml::yaml.load_file
#'     }
#'   }
#' @format NULL
#' @usage NULL
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
    file_extension = ".yml",
    path = NULL,
    string = NULL,

    initialize = function(file_extension = ".yml", path = NULL, string = NULL) {
      self$file_extension <- file_extension
      if (is.null(path)) {
        self$path <- paste0(cassette_path(), "/", basename(tempfile()), file_extension)
      } else {
        self$path <- paste0(cassette_path(), "/", path, file_extension)
      }
      self$string <- string
    },

    # Serializes the given hash using internal fxn write_yaml
    serialize = function(x, path, bytes) {
      # @param [list] x the object to serialize
      # @param [character] the file path
      # @param [logical] whether to preserve exact body bytes or not
      # @return [String] the YAML string to write to disk
      #write_yaml(x, self$path)
      function(x, path, bytes) {
        write_yaml(x, path, bytes)
      }
    },

    # Deserializes the given string using yaml::yaml.load
    deserialize_string = function(string = NULL) {
      # @param [String] string the YAML string
      # @return [Hash] the deserialized object, an R list
      if (is.null(self$string)) str <- string else self$string
      if (is.null(str)) stop("Must pass a string", call. = FALSE)
      yaml::yaml.load(str)
    },

    # Deserializes the given string using yaml::yaml.load_file
    deserialize_path = function() {
      # @param [String] string path to a YAML file
      # @return [Hash] the deserialized object, an R list

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
            z$response$body$string <-
              rawToChar(base64enc::base64decode(z$response$body$string))
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
