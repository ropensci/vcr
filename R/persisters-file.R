#' @title File system persister
#' @description The only built-in cassette persister. Persists cassettes
#' to the file system.
#' @keywords internal
#' @details
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{storage_location()}}{
#'       Get storage location
#'     }
#'     \item{\code{absolute_path_to_file()}}{
#'       Get absolute path to the `storage_location`
#'     }
#'   }
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
    #' @field file_name (character) the file name, not whole path
    file_name = NULL,
    #' @field write_fxn (character) fxn to use for writing to disk
    write_fxn = NULL,
    #' @field content (character) content to record to a cassette
    content = NULL,
    #' @field path (character) storage directory for cassettes
    path = NULL,
    #' @field write2disk (character) write to disk or make a new FileSystem
    write2disk = FALSE,

    #' @description Create a new `FileSystem` object
    #' @param file_name (character) the file name, not whole path
    #' @param write_fxn (character) fxn to use for writing to disk
    #' @param content (character) content to record to a cassette
    #' @param path (character) storage directory for cassettes
    #' @param write2disk (logical) write to disk or just make a new FileSystem
    #' object. Default: `FALSE`
    #' @return A new `FileSystem` object
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

    #' @description Gets the cassette for the given storage key (file name)
    #' @param file_name (character) the file name, not whole path
    #' @return named list, from `yaml::yaml.load_file`
    get_cassette = function(file_name = NULL) {
      # @param [String] file_name the file name
      # @return [String] the cassette content
      file_name <- if (is.null(file_name)) self$file_name else file_name
      if (is.null(file_name)) stop('No file name provided', call. = FALSE)
      path <- private$absolute_path_to_file(self$path, file_name)
      if (!file.exists(path)) stop("File doesn't exist", call. = FALSE)
      yaml::yaml.load_file(path)
    },

    #' @description Checks if a cassette is empty or not
    #' @return logical
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

    #' @description Sets the cassette for the given storage key (file name)
    #' @param file_name (character) the file name, not whole path
    #' @param content (character) content to record to a cassette
    #' @return no return; writes to disk
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
