vcr_log_env <- new.env()

#' vcr log file setup
#'
#' @export
#' @name vcr_logging
#' @keywords internal
#' @param file (character) a file path, required
#' @param message (character) a message to log
#' @param overwrite (logical) whether or not to overwrite the file at
#' 'file' if it already exists. Default: `TRUE`
#' @param include_date (logical) include date and time in each log entry.
#' Default: `FALSE`
#' @examples
#' # user workflow
#' vcr_configuration()
#' vcr_configure(log = TRUE)
#' vcr_configuration()
#'
#' readLines("vcr.log") # empty
#'
#' # log messages
#' vcr_log_info("hello world!")
#' readLines("vcr.log")
#' vcr_log_info("foo bar")
#' readLines("vcr.log")
#' ## many messages
#' vcr_log_info(c("brown cow", "blue horse"))
#' readLines("vcr.log")
#' vcr_log_info(c("brown cow", "blue horse", "green goat"))
#' readLines("vcr.log")
#'
#' # standalone workflow
#' # set a file to log to
#' vcr_log_file((f <- tempfile()))
#' readLines(f) # empty
#'
#' # log messages
#' vcr_log_info("hello world!")
#' readLines("vcr.log")
#' vcr_log_info("foo bar")
#' readLines("vcr.log")
#'
#' # cleanup
#' # unlink(f)

# FIXME: add
#    indentation = '  ' * indentation_level

#' @export
#' @rdname vcr_logging
vcr_log_file <- function(file, overwrite = TRUE) {
  stopifnot(inherits(file, 'character'))
  if (file != "console") {
    if (!file.exists(file)) {
      file.create(file)
    } else if (file.exists(file) && overwrite) {
      file.remove(file)
      file.create(file)
    } else {
      stop('file exists and overwrite=FALSE')
    }
  }
  # save file name
  vcr_log_env$file <- file
  return(TRUE)
}

#' @export
#' @rdname vcr_logging
vcr_log_info <- function(message, include_date = TRUE) {
  if (include_date) message <- paste(as.character(Sys.time()), "-", message)
  message <- paste(make_prefix(), "-", message)
  vcr_log_write(message)
}

make_prefix <- function() {
  sprintf("[%s: '%s']", vcr_c$log_opts$log_prefix,
    vcr__env$current_cassette)
}

vcr_log_write <- function(message) {
  if (vcr_c$log) {
    if (is.null(vcr_log_env$file)) {
      stop("no connection set up to write to, see ?vcr_logging")
    }
    if (vcr_log_env$file == "console") {
      cat(message, sep = "\n", append = TRUE)
    } else {
      cat(message, sep = "\n", file = vcr_log_env$file, append = TRUE)
    }
  }
}

trailing_newline <- function(str) {
  if (grepl("\n$", str)) str else paste0(str, "\n")
}
