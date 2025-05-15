decode_sensitive <- function(x) {
  if (!is.null(the$config$filter_sensitive_data) && !is_empty(x)) {
    fsd <- the$config$filter_sensitive_data
    for (i in seq_along(fsd)) {
      x <- gsub(names(fsd)[i], fsd[[i]], x, fixed = TRUE)
    }
  }
  return(x)
}
encode_sensitive <- function(x) {
  if (is.null(x)) {
    return()
  }

  fsd <- the$config$filter_sensitive_data
  for (i in seq_along(fsd)) {
    if (nchar(fsd[[i]]) > 0) {
      x <- gsub(fsd[[i]], names(fsd)[i], x, fixed = TRUE)
    }
  }

  fsdr <- the$config$filter_sensitive_data_regex
  for (i in seq_along(fsdr)) {
    if (nchar(fsdr[[i]]) > 0) {
      x <- gsub(fsdr[[i]], names(fsdr)[i], x, fixed = FALSE)
    }
  }
  return(x)
}
