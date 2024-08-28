#' Remove or replace query parameters
#' @noRd
query_params_remove <- function(int) {
  h <- vcr_c$filter_query_parameters
  if (!is.null(h)) {
    if (is.null(names(h))) toremove <- unlist(h)
    if (!is.null(names(h))) toremove <- unname(unlist(h[!nzchar(names(h))]))
    # remove zero length strings
    toremove <- Filter(nzchar, toremove)
    for (i in seq_along(toremove)) {
      int <- lapply(int, function(b) {
        b$request$uri <- drop_param(b$request$uri, toremove[i])
        return(b)
      })
    }

    toreplace <- h[nzchar(names(h))]
    if (length(toreplace)) {
      for (i in seq_along(toreplace)) {
        int <- lapply(int, function(b) {
          vals <- toreplace[[i]]
          val <- if (length(vals) == 2) vals[2] else vals[1]
          b$request$uri <-
            replace_param(b$request$uri, names(toreplace)[i], val)
          return(b)
        })
      }
    }
  }
  return(int)
}

#' Put back query parameters
#' @details Ignore character strings as we can't put back completely removed
#' query parameters
#' @noRd
query_params_put_back <- function(int) {
  h <- vcr_c$filter_query_parameters
  if (!is.null(h)) {
    toputback <- h[nzchar(names(h))]
    if (length(toputback)) {
      for (i in seq_along(toputback)) {
        int$http_interactions <- lapply(int$http_interactions, function(b) {
          vals <- toputback[[i]]
          if (length(vals) == 2) {
            b$request$uri <-
              replace_param_with(b$request$uri, names(toputback)[i], vals[2], vals[1])
          } else {
            b$request$uri <-
              replace_param(b$request$uri, names(toputback)[i], vals)
          }
          return(b)
        })
      }
    }
  }
  return(int)
}

query_params_remove_str <- function(uri) {
  h <- vcr_c$filter_query_parameters
  if (!is.null(h)) {
    if (is.null(names(h))) toremove <- unlist(h)
    if (!is.null(names(h))) toremove <- unname(unlist(h[!nzchar(names(h))]))
    toremove <- Filter(nzchar, toremove)
    for (i in seq_along(toremove)) uri <- drop_param(uri, toremove[i])

    toreplace <- h[nzchar(names(h))]
    if (length(toreplace)) {
      for (i in seq_along(toreplace)) {
        vals <- toreplace[[i]]
        if (length(vals) == 2) {
          uri <-
            replace_param_with(uri, names(toreplace)[i], vals[2], vals[1])
        } else {
          uri <- replace_param(uri, names(toreplace)[i], vals)
        }
      }
    }
  }
  return(uri)
}
list2str <- function(w) {
  paste(names(w), unlist(unname(w)), sep="=", collapse="&")
}
buildurl <- function(x) {
  x$parameter <- list2str(x$parameter)
  url <- urltools::url_compose(x)
  # trim trailing ?
  sub("\\?$", "", url)
}
# drop_param(url="https://hb.opencpu.org/get?foo=bar&baz=3&z=4", name="z")
# => "https://hb.opencpu.org/get?foo=bar&baz=3"
drop_param <- function(url, name) {
  assert(name, "character")
  stopifnot("can only drop one name at a time" = length(name) == 1)
  z <- parseurl(url)
  z$parameter[[name]] <- NULL
  buildurl(z)
}
# replace_param(url="https://hb.opencpu.org/get?foo=5", name="foo", value=4)
# => "https://hb.opencpu.org/get?foo=4"
# # return param value unchanged if param name not found
# replace_param(url="https://hb.opencpu.org/get?bar=3", name="foo", value=4)
# => "https://hb.opencpu.org/get?bar=3"
# # No params at all
# replace_param(url="https://hb.opencpu.org/get", name="foo", value=4)
# => "https://hb.opencpu.org/get"
replace_param <- function(url, name, value) {
  assert(name, "character")
  stopifnot("can only replace one name at a time" = length(name) == 1)
  z <- parseurl(url)
  if (!is.list(z$parameter)) return(url)
  if (is.null(z$parameter[[name]])) return(url)
  z$parameter[[name]] <- value
  buildurl(z)
}

replace_param_with <- function(url, name, fake, real) {
  assert(name, "character")
  stopifnot("can only replace one name at a time" = length(name) == 1)
  z <- parseurl(url)
  if (!is.list(z$parameter)) return(url)
  if (is.null(z$parameter[[name]])) return(url)
  z$parameter[[name]] <- sub(fake, real, z$parameter[[name]])
  buildurl(z)
}
