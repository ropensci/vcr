pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

last <- function(x) {
  if (length(x) == 0) {
    return(list())
  } else {
    x[length(x)][1]
  }
}

errmssg <- "use_cassette requires a block.\nIf you cannot wrap your code in a block, use\ninsert_cassette / eject_cassette instead."

compact <- function(x) Filter(Negate(is.null), x)

`%||%` <- function(x, y) {
  if (is.null(x) || all(nchar(x) == 0) || length(x) == 0) y else x
}

stract <- function(str, pattern) regmatches(str, regexpr(pattern, str))

assert <- function(x, y) {
  if (!is.null(x)) {
    if (!inherits(x, y)) {
      stop(deparse(substitute(x)), " must be of class ",
           paste0(y, collapse = ", "), call. = FALSE)
    }
  }
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

check_for_a_pkg <- function(x) {
  if (!requireNamespace(x, quietly = TRUE)) {
    stop("Please install ", x, call. = FALSE)
  } else {
    invisible(TRUE)
  }
}

has_internet <- function() {
  z <- try(suppressWarnings(readLines('https://www.google.com', n = 1)),
    silent = TRUE)
  !inherits(z, "try-error")
}
