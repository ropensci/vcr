encode_uri <- function(
  uri,
  filter = the$config$filter_query_parameters,
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

  uri <- encode_sensitive(uri)

  uri
}

decode_uri <- function(uri, filter = the$config$filter_query_parameters) {
  encode_uri(uri, filter, flip = TRUE)
}

# drop_param(url="https://hb.opencpu.org/get?foo=bar&baz=3&z=4", name="z")
# => "https://hb.opencpu.org/get?foo=bar&baz=3"
drop_param <- function(url, name) {
  assert(name, "character")
  stopifnot("can only drop one name at a time" = length(name) == 1)
  z <- parseurl(url)
  if (!is.list(z$parameter)) return(url)
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

# Helpers ----------------------------------------------------------------------

parseurl <- function(x) {
  tmp <- urltools::url_parse(x)
  tmp <- as.list(tmp)
  if (!is.na(tmp$parameter)) {
    tmp$parameter <- sapply(
      strsplit(tmp$parameter, "&")[[1]],
      function(z) {
        zz <- strsplit(z, split = "=")[[1]]
        as.list(stats::setNames(zz[2], zz[1]))
      },
      USE.NAMES = FALSE
    )
  }
  tmp
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
