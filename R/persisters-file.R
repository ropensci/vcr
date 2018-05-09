#' File system persister
#'
#' @keywords internal
#' @param file_name (character) Just the file name, not whole path
#' @param content (character) content to record to a cassette
#' @param path (character) Storage directory for cassettes
#' @param write2disk (logical) write to disk or just make a new FileSystem
#' object. Default: `FALSE`
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
#'     \item{\code{is_empty()}}{
#'       Checks if a cassette is empty or not. Returns boolean
#'     }
#'   }
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{storage_location()}}{
#'       Get storage location
#'     }
#'     \item{\code{absolute_path_to_file()}}{
#'       Get absolute path to the `storage_location`
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' vcr_configure(dir = tempdir())
#'
#' (yy <- FileSystem$new(file_name = "file4014931b21b.yml"))
#' yy$set_cassette(content = "hello world!")
#'
#' # is empty?
#' yy$is_empty()
#'
#' # get cassette
#' yy$get_cassette(file_name = "file4014931b21b.yml")
#'
#' # clenaup
#' unlink(file.path(tempdir(), "file4014931b21b.yml"))
#' }
FileSystem <- R6::R6Class("FileSystem",
  public = list(
    file_name = NULL,
    write_fxn = NULL,
    content = NULL,
    path = NULL,
    write2disk = FALSE,

    initialize = function(file_name = NULL, write_fxn = NULL,
                          content = NULL, path = NULL,
                          write2disk = FALSE) {
      self$file_name <- file_name
      self$content <- content
      self$write_fxn <- write_fxn
      self$write2disk <- write2disk
      self$path <- if (is.null(path)) cassette_path() else path

      # write to disk
      if (self$write2disk) self$write_fxn(self$content, self$path)
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

    is_empty = function() {
      if (is.null(self$file_name)) stop('No file name provided', call. = FALSE)
      path <- private$absolute_path_to_file(self$path, self$file_name)
      if (!file.exists(path)) return(TRUE)
      if (is.null(yaml::yaml.load_file(path))) {
        return(TRUE)
      } else {
        return(FALSE)
      }
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
