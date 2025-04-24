pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

compact <- function(x) Filter(Negate(is.null), x)

`%||%` <- function(x, y) {
  if (missing(x) || is.null(x) || all(nchar(x) == 0) || length(x) == 0) y else x
}

stract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

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

merge_list <- function(x, y, ...) {
  if (length(x) == 0) return(y)
  if (length(y) == 0) return(x)
  z <- match(names(y), names(x))
  z <- is.na(z)
  if (any(z)) {
    x[names(y)[which(z)]] = y[which(z)]
  }
  x
}

can_rawToChar <- function(x) {
  z <- tryCatch(rawToChar(x), error = function(e) e)
  return(!inherits(z, "error"))
}
can_charToRaw <- function(x) {
  z <- tryCatch(charToRaw(x), error = function(e) e)
  return(!inherits(z, "error"))
}

check_request_matchers <- function(x) {
  mro <- c("method", "uri", "headers", "host", "path", "body", "query")
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

to_base64 <- function(x) {
  x <- jsonlite::base64_enc(x)

  # Split into lines of 80 characters
  chunk_size <- 80
  length <- nchar(x)
  if (length < chunk_size) {
    return(x)
  }

  num_chunks <- ceiling(length / 80)
  start_positions <- seq(1, by = 80, length.out = num_chunks)
  end_positions <- pmin(start_positions + chunk_size - 1, length)

  lines <- substring(x, start_positions, end_positions)
  paste0(lines, collapse = "\n")
}

from_base64 <- function(x) {
  x <- gsub("[\r\n]", "", x)
  jsonlite::base64_dec(x)
}

cur_time <- function(tz = "") {
  format(Sys.time(), format = "%Y-%m-%d %H:%M:%S", tz = tz)
}
