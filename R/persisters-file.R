#' File system persister
#'
#' @description  The only built-in cassette persister. Persists cassettes
#' to the file system.
#'
#' @examples
#' nn <- list.files(cassette_path(), full.names = TRUE)[1]
#' (yy <- FileSystem$new(file_name = nn))
FileSystem <- R6::R6Class("FileSystem",
  public = list(
    file_name = NULL,
    content = NULL,
    initialize = function(file_name = NULL, content = NULL) {
      self$file_name <- file_name
      self$content <- content
    },
    # Gets the cassette for the given storage key (file name).
    get_cassette = function(file_name) {
      # @param [String] file_name the file name
      # @return [String] the cassette content
      path <- path.expand(self$file_name)
      if (!file.exists(path)) stop("File doesn't exist", call. = FALSE)
      yaml::yaml.load_file(path)
    },
    # Sets the cassette for the given storage key (file name).
    set_cassette = function(file_name, content) {
      # @param [String] file_name the file name
      # @param [String] content the content to store
      path <- path.expand(self$file_name)
      directory <- dirname(path)
      if (!file.exists(directory)) {
        dir.create(directory, recursive = TRUE, showWarnings = TRUE)
      }
      cat(yaml::as.yaml(content), file = path)
    }
  )
)
