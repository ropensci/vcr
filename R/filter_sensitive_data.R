# filter_sensitive_data replacement
# FIXME: eventually move to higher level so that this happens
#   regardless of serializer
sensitive_put_back <- function(x) {
  if (!is.null(vcr_c$filter_sensitive_data)) {
    fsd <- vcr_c$filter_sensitive_data
    for (i in seq_along(fsd)) {
      x <- gsub(names(fsd)[i], fsd[[i]], x)
    }
  }
  return(x)
}
sensitive_remove <- function(x) {
  fsd <- vcr_c$filter_sensitive_data
  if (!is.null(fsd)) {
    for (i in seq_along(fsd)) {
      if (nchar(fsd[[i]]) > 0) {
        x <- gsub(fsd[[i]], names(fsd)[i], x)
      }
    }
  }
  return(x)
}
