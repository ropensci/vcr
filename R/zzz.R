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
    if (!class(x)[1L] %in% y) {
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
