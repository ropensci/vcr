pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

errmssg <- "use_cassette requires a block.\nIf you cannot wrap your code in a block, use\ninsert_cassette / eject_cassette instead."

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

has_internet <- function() {
  z <- try(
    suppressWarnings(readLines('https://www.google.com', n = 1)),
    silent = TRUE
  )
  !inherits(z, "try-error")
}

can_rawToChar <- function(x) {
  z <- tryCatch(rawToChar(x), error = function(e) e)
  return(!inherits(z, "error"))
}
can_charToRaw <- function(x) {
  z <- tryCatch(charToRaw(x), error = function(e) e)
  return(!inherits(z, "error"))
}

stp <- function(...) stop(..., call. = FALSE)
check_cassette_name <- function(x) {
  if (length(x) != 1) stp("cassette name must be a single string")

  if (grepl("\\s", x)) stp("no spaces allowed in cassette names")
  if (grepl("\\.yml$|\\.yaml$", x))
    stp("don't include a cassette path extension")
  # the below adapted from fs::path_sanitize, which adapted
  # from the npm package sanitize-filename
  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"
  if (grepl(illegal, x))
    stp(
      "none of the following characters allowed in cassette ",
      "names: (/, ?, <, >, \\, :, *, |, and \")"
    )
  if (grepl(control, x)) stp("no control characters allowed in cassette names")
  if (grepl(reserved, x)) stp("cassette names can not be simply ., .., etc.")
  if (grepl(windows_reserved, x))
    stp("cassette names can not have reserved windows strings")
  if (grepl(windows_trailing, x))
    stp("cassette names can not have a trailing .")
  if (nchar(x) > 255) stp("cassette name can not be > 255 characters")
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

sup_cond <- function(quiet, fun, cond = suppressMessages) {
  if (quiet) cond(fun) else force(fun)
}
sup_mssg <- function(quiet, fun) sup_cond(quiet, fun)
sup_warn <- function(quiet, fun) sup_cond(quiet, fun, suppressWarnings)

dims <- function(x) length(dim(x))

dir_create <- function(path) {
  dir.create(path, showWarnings = FALSE, recursive = TRUE)
}
