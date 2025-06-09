encode_uri <- function(
  uri,
  filter = the$config$filter_query_parameters,
  flip = FALSE
) {
  is_named <- names2(filter) != ""

  params <- as.list(curl::curl_parse_url(uri)$params)

  to_remove <- unlist(filter[!is_named])
  params <- params[setdiff(names(params), to_remove)]

  to_replace <- intersect(names(params), names(filter))
  for (nm in to_replace) {
    val <- filter[[nm]]
    if (length(val) == 2) {
      if (flip) {
        params[[nm]] <- sub(val[[2]], val[[1]], params[[nm]])
      } else {
        params[[nm]] <- sub(val[[1]], val[[2]], params[[nm]])
      }
    } else {
      params[[nm]] <- val
    }
  }

  # TODO: remove once curl 6.3.1 is on CRAN
  if (length(params) == 0) {
    uri <- curl::curl_modify_url(uri, query = "")
  } else {
    uri <- curl::curl_modify_url(uri, params = unlist(params))
  }

  uri <- encode_sensitive(uri)
  uri
}

decode_uri <- function(uri, filter = the$config$filter_query_parameters) {
  encode_uri(uri, filter, flip = TRUE)
}
