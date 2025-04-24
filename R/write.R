# dedup header keys so we have unique yaml keys
# (x <- list(b = "foo", c = list(a = 5, a = 6)))
# (x <- list(b = "foo", a = 5))
# (x <- list(b = "foo", a = 5, a = 6))
# dedup_keys(x)
dedup_keys <- function(x) {
  if (length(x) == 0 || is.null(x)) return(x)
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

encode_interactions <- function(x, preserve_bytes = FALSE) {
  interactions <- lapply(x, encode_interaction, preserve_bytes)
  list(
    http_interactions = interactions,
    recorded_with = pkg_versions()
  )
}

encode_interaction <- function(x, preserve_bytes) {
  list(
    request = list(
      method = x$request$method,
      uri = encode_uri(x$request$uri),
      body = encode_body(x$request$body, NULL, preserve_bytes),
      headers = encode_headers(x$request$headers, "request")
    ),
    response = list(
      status = x$response$status,
      headers = encode_headers(x$response$headers, "response"),
      body = encode_body(x$response$body, x$response$disk, preserve_bytes)
    ),
    recorded_at = paste0(cur_time(tz = "GMT"), " GMT")
  )
}

encode_headers <- function(headers, type = c("request", "response")) {
  type <- arg_match(type)

  headers <- dedup_keys(headers)

  headers <- switch(
    type,
    request = headers_remove(headers, vcr_c$filter_request_headers),
    response = headers_remove(headers, vcr_c$filter_response_headers)
  )

  if (type == "request") {
    headers <- request_headers_redact(headers)
  }

  headers <- sensitive_remove(headers)

  headers <- unclass(headers)
  headers
}

encode_body <- function(body, file, preserve_bytes = FALSE) {
  if (is.null(body)) {
    list(encoding = "", string = "")
  } else if (is.raw(body) || preserve_bytes) {
    compact(list(
      encoding = "",
      base64_string = to_base64(body),
      file = file
    ))
  } else {
    compact(list(
      encoding = "",
      string = sensitive_remove(body) %||% "",
      file = file
    ))
  }
}

pkg_versions <- function() {
  paste(
    paste0("vcr/", utils::packageVersion("vcr")),
    paste0("webmockr/", utils::packageVersion("webmockr")),
    sep = ", "
  )
}
