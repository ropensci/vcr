#' File system persister
#'
#' @keywords internal
#' @param file_name (character) Just he fiile name, not whole path
#' @param content (character) content to record to a cassette
#' @param path (character) Storage directory for cassettes
#' @details The only built-in cassette persister. Persists cassettes
#' to the file system.
#'
#' \strong{Methods}
#'   \describe{
#'     \item{\code{get_cassette(file_name = NULL, content = NULL, path = NULL)}}{
#'       Gets the cassette for the given storage key (file name).
#'     }
#'     \item{\code{set_cassette(file_name = NULL, content)}}{
#'       Sets the cassette for the given storage key (file name).
#'     }
#'   }
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{storage_location()}}{
#'       Get storage location
#'     }
#'     \item{\code{absolute_path_to_file()}}{
#'       Get absolute path to the \code{storage_location}
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' nn <- list.files(cassette_path())
#'
#' (yy <- FileSystem$new())
#' yy$get_cassette(file_name = "file4014931b21b.yml")
#'
#' (yy <- FileSystem$new(file_name = "file4014931b21b.yml"))
#' yy$get_cassette()
#'
#' (yy <- FileSystem$new(file_name = "file4014931b21b.yml"))
#' yy$set_cassette(content = "hello world!")
#' }
FileSystem <- R6::R6Class("FileSystem",
  public = list(
    file_name = NULL,
    content = NULL,
    path = NULL,

    initialize = function(file_name = NULL, content = NULL, path = NULL) {
      self$file_name <- file_name
      self$content <- content
      self$path <- if (is.null(path)) cassette_path() else path
    },

    # Gets the cassette for the given storage key (file name).
    get_cassette = function(file_name = NULL) {
      # @param [String] file_name the file name
      # @return [String] the cassette content
      file_name <- if (is.null(file_name)) self$file_name else file_name
      if (is.null(file_name)) stop('No file name provided', call. = FALSE)
      path <- private$absolute_path_to_file(self$path, file_name)
      if (!file.exists(path)) stop("File doesn't exist", call. = FALSE)
      yaml::yaml.load_file(path)
    },

    # Sets the cassette for the given storage key (file name).
    set_cassette = function(file_name = NULL, content) {
      # @param [String] file_name the file name
      # @param [String] content the content to store
      file_name <- if (is.null(file_name)) self$file_name else file_name
      if (is.null(file_name)) stop('No file name provided', call. = FALSE)
      path <- private$absolute_path_to_file(self$path, file_name)

      directory <- dirname(path)
      if (!file.exists(directory)) {
        dir.create(directory, recursive = TRUE, showWarnings = TRUE)
      }
      cat(yaml::as.yaml(content), file = path)
    }
  ),

  private = list(
    storage_location = function() {
      self$path
    },

    absolute_path_to_file = function(x, y) {
      if (is.null(self$path)) {
        NULL
      } else {
        path.expand(file.path(x, y))
      }
    }
  )
)
