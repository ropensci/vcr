#' Remove or replace query parameters
#' @noRd
query_params_remove <- function(
  uri,
  filter = vcr_c$filter_query_parameters,
  flip = FALSE
) {
  is_named <- names2(filter) != ""

  to_remove <- filter[!is_named]
  for (i in seq_along(to_remove)) {
    uri <- drop_param(uri, to_remove[[i]])
  }

  to_replace <- filter[is_named]
  for (i in seq_along(to_replace)) {
    val <- to_replace[[i]]
    if (length(val) == 2) {
      if (flip) {
        uri <- replace_param_with(uri, names(to_replace)[[i]], val[2], val[1])
      } else {
        uri <- replace_param_with(uri, names(to_replace)[[i]], val[1], val[2])
      }
    } else {
      uri <- replace_param(uri, names(to_replace)[[i]], val)
    }
  }

  uri
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
              replace_param_with(
                b$request$uri,
                names(toputback)[i],
                vals[2],
                vals[1]
              )
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

list2str <- function(w) {
  paste(names(w), unlist(unname(w)), sep = "=", collapse = "&")
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
