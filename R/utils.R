compact <- function(x) Filter(Negate(is.null), x)

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(
        deparse(substitute(x)),
        " must be of class ",
        paste0(y, collapse = ", "),
        call. = FALSE
      )
    }
  }
  invisible(x)
}

can_rawToChar <- function(x) {
  z <- tryCatch(rawToChar(x), error = function(e) e)
  return(!inherits(z, "error"))
}

check_request_matchers <- function(
  x,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.null(x)) {
    return()
  }
  vals <- c("method", "uri", "headers", "host", "path", "body", "query")
  arg_match(
    x,
    vals,
    error_arg = error_arg,
    error_call = error_call,
    multiple = TRUE
  )
}

check_record_mode <- function(
  x,
  error_arg = caller_arg(x),
  error_call = caller_env()
) {
  if (is.null(x)) {
    return()
  }
  vals <- c("none", "once", "new_episodes", "all")
  arg_match(x, vals, error_arg = error_arg, error_call = error_call)
}

dir_create <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}

cur_time <- function(tz = "") {
  format(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = tz)
}

pkg_versions <- function() {
  paste(
    paste0("vcr/", utils::packageVersion("vcr")),
    paste0("webmockr/", utils::packageVersion("webmockr")),
    sep = ", "
  )
}
