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
  if (!is.null(vcr_c$filter_sensitive_data)) {
    fsd <- vcr_c$filter_sensitive_data
    for (i in seq_along(fsd)) {
      x <- gsub(fsd[[i]], names(fsd)[i], x)
    }
  }
  return(x)
}
