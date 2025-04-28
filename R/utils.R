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

check_request_matchers <- function(x) {
  mro <- c(
    "method",
    "uri",
    "headers",
    "host",
    "path",
    "body",
    "body_json",
    "query"
  )
  if (!all(x %in% mro)) {
    stop(
      "1 or more 'match_requests_on' values (",
      paste0(x, collapse = ", "),
      ") is not in the allowed set: ",
      paste0(mro, collapse = ", "),
      call. = FALSE
    )
  }
  x
}

check_record_mode <- function(x) {
  stopifnot(length(x) == 1, is.character(x))
  recmodes <- c("none", "once", "new_episodes", "all")
  if (!x %in% recmodes) {
    stop(
      "'record' value of '",
      x,
      "' is not in the allowed set: ",
      paste0(recmodes, collapse = ", "),
      call. = FALSE
    )
  }
  x
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
