write_yaml <- function(x, file, bytes) {
  write_header(file)
  lapply(x, write_interactions, file = file, bytes = bytes)
}

write_json <- function(x, file, bytes) {
  lapply(x, write_interactions_json, file = file, bytes = bytes)
}

write_header <- function(file) {
  cat("http_interactions:", sep = "\n", file = file)
}

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

prep_interaction <- function(x, file, bytes) {
  list(
    list(
      request = list(
        method = x$request$method,
        uri = query_params_remove(x$request$uri),
        body = encode_body(x$request$body, NULL, bytes),
        headers = dedup_keys(x$request$headers)
      ),
      response = list(
        status = x$response$status,
        headers = dedup_keys(x$response$headers),
        body = encode_body(x$response$body, x$response$disk, bytes)
      ),
      recorded_at = paste0(format(Sys.time(), tz = "GMT"), " GMT"),
      recorded_with = pkg_versions()
    )
  )
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
      string = body %||% "",
      file = file
    ))
  }
}

# param x: a list with "request" and "response" slots
# param file: a file path
# param bytes: logical, whether to preserve exact bytes or not
write_interactions <- function(x, file, bytes) {
  z <- prep_interaction(x, file, bytes)
  z <- headers_remove(z)
  z <- request_headers_redact(z)
  tmp <- yaml::as.yaml(z)
  tmp <- sensitive_remove(tmp)
  cat(tmp, file = file, append = TRUE)
}

write_interactions_json <- function(x, file, bytes) {
  z <- prep_interaction(x, file, bytes)
  z <- headers_remove(z)
  z <- request_headers_redact(z)
  z <- headers_unclass(z)
  # combine with existing data on same file, if any
  on_disk <- invisible(tryCatch(
    jsonlite::fromJSON(file, FALSE),
    error = function(e) e
  ))
  if (!inherits(on_disk, "error") && is.list(on_disk)) {
    z <- c(on_disk$http_interactions, z)
  }
  tmp <- jsonlite::toJSON(
    list(http_interactions = z),
    auto_unbox = TRUE,
    pretty = vcr_c$json_pretty
  )
  tmp <- sensitive_remove(tmp)
  cat(paste0(tmp, "\n"), file = file)
}

pkg_versions <- function() {
  paste(
    paste0("vcr/", utils::packageVersion("vcr")),
    paste0("webmockr/", utils::packageVersion("webmockr")),
    sep = ", "
  )
}

get_body <- function(x) {
  if (is.null(x)) '' else x
}
