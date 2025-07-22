#' Configure vcr logging
#'
#' By default, logging is disabled, but you can easily enable for the
#' entire session with `vcr_configure_log()` or for just one test with
#' `local_vcr_configure_log()`.
#'
#' @param log Should we log important vcr things?
#' @param file A path or connection to log to
#' @param include_date (boolean) Include date and time in each log entry.
#' @param log_prefix "Cassette". We insert the cassette name after this prefix,
#'     followed by the rest of the message.
#' @export
#' @examples
#' # The default logs to stderr()
#' vcr_configure_log()
#'
#' # But you might want to log to a file
#' vcr_configure_log(file = file.path(tempdir(), "vcr.log"))
vcr_configure_log <- function(
  log = TRUE,
  file = stderr(),
  include_date = NULL,
  log_prefix = "Cassette"
) {
  # sink() works by redefining the values of stdout() and stderr()
  # so we can not store those values
  if (identical(file, stderr())) {
    file <- function() stderr()
    include_date <- include_date %||% FALSE
  } else if (identical(file, stdout())) {
    file <- function() stdout()
    include_date <- include_date %||% FALSE
  } else {
    include_date <- include_date %||% TRUE
  }

  vcr_configure(
    log = log,
    log_opts = list(
      file = file,
      include_date = include_date,
      log_prefix = log_prefix
    )
  )
}

#' @export
#' @rdname vcr_configure_log
#' @inheritParams local_vcr_configure
local_vcr_configure_log <- function(
  log = TRUE,
  file = stderr(),
  include_date = NULL,
  log_prefix = "Cassette",
  frame = parent.frame()
) {
  old <- vcr_configure_log(
    log = log,
    file = file,
    include_date = include_date,
    log_prefix = log_prefix
  )
  defer(exec(vcr_configure, !!!old), frame)

  invisible()
}

vcr_log_sprintf <- function(message, ...) {
  if (!the$config$log) {
    return(invisible())
  }

  message <- sprintf(message, ...)

  if (the$config$log_opts$include_date) {
    date <- cur_time()
  } else {
    date <- NULL
  }

  if (cassette_active()) {
    cassette_name <- current_cassette()$name
  } else {
    cassette_name <- "<none>"
  }
  prefix <- sprintf("[%s: %s]", the$config$log_opts$log_prefix, cassette_name)

  message <- paste(c(date, prefix, message), collapse = " ")

  file <- the$config$log_opts$file
  if (is.function(file)) {
    file <- file()
  }

  cat(message, sep = "\n", file = file, append = TRUE)
}
