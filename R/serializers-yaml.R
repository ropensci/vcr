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
#' (yy <- YAML$new())
#' library("httr")
#' x <- GET("http://httpbin.org/get")
#' yy$serialize(x)
#' yy$deserialize_path()
#' # yy$deserialize_string() # if passed on instantiation
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
    serialize = function(x, path) {
      # @param [Hash] x the object to serialize
      # @return [String] the YAML string
      #write_yaml(x, self$path)
      function(x, path) {
        write_yaml(x, path)
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
      tmp <- yaml::yaml.load_file(self$path)
      if (is.null(tmp)) list() else tmp
    }
  )
)
