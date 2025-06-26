encode_headers <- function(headers, type = c("request", "response")) {
  type <- arg_match(type)

  headers <- dedup_keys(headers)

  headers <- switch(
    type,
    request = headers_remove(headers, the$config$filter_request_headers),
    response = headers_remove(headers, the$config$filter_response_headers)
  )

  if (type == "request") {
    headers <- request_headers_redact(headers)
  }

  headers <- lapply(headers, encode_sensitive)

  headers <- unclass(headers)
  headers
}

decode_headers <- function(headers) {
  if (is.null(headers)) {
    list()
  } else {
    lapply(headers, decode_sensitive)
  }
}

headers_remove <- function(headers, filter) {
  is_named <- names2(filter) != ""

  to_remove <- filter[!is_named]
  for (i in seq_along(to_remove)) {
    headers[[to_remove[[i]]]] <- NULL
  }

  to_replace <- filter[is_named]
  to_replace <- to_replace[intersect(names(headers), names(to_replace))]
  for (i in seq_along(to_replace)) {
    headers[[names(to_replace)[[i]]]] <- to_replace[[i]]
  }

  headers
}

request_headers_redact <- function(headers) {
  # TODO: remove once we depend on modern httr2
  to_redact <- union(attr(headers, "redact"), "authorization")
  matches <- match(tolower(to_redact), tolower(names(headers)))
  matches <- matches[!is.na(matches)]

  if (length(matches) == 0) {
    headers
  } else {
    headers[-matches]
  }
}

# dedup header keys so we have unique yaml keys
# (x <- list(b = "foo", c = list(a = 5, a = 6)))
# (x <- list(b = "foo", a = 5))
# (x <- list(b = "foo", a = 5, a = 6))
# dedup_keys(x)
dedup_keys <- function(x) {
  if (length(x) == 0 || is.null(x)) {
    return(x)
  }
  nms <- names(x)
  # if repeats, collapse dups under single name
  if (length(unique(nms)) != length(nms)) {
    x <- split(x, nms)
    for (i in seq_along(x)) {
      if (length(x[[i]]) > 1) {
        x[[i]] <- unlist(unname(x[[i]]))
      } else {
        x[[i]] <- unlist(unname(x[[i]]))
      }
    }
  }
  return(x)
}
