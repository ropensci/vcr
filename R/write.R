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

str_breaks <- function(x) {
  z <- str_splitter(x, 80L)
  paste0(z, collapse = "\n")
}

prep_interaction <- function(x, file, bytes) {
  assert(x, c("list", "HTTPInteraction"))
  assert(file, "character")
  if (is.raw(x$response$body)) bytes <- TRUE
  body <- if (bytes || is.raw(x$response$body)) {
    bd <- get_body(x$response$body)
    if (!is.raw(bd)) bd <- charToRaw(bd)
    tmp <- base64enc::base64encode(bd)
    str_breaks(tmp)
  } else {
    get_body(x$response$body)
  }
  if (length(body) == 0 || !nzchar(body)) body <- ""
  res = list(
    list(
      request = list(
        method = x$request$method,
        uri = x$request$uri,
        body = list(
          encoding = "",
          string = get_body(x$request$body)
        ),
        headers = dedup_keys(x$request$headers)
      ),
      response = list(
        status = x$response$status,
        headers = dedup_keys(x$response$headers),
        body = list(
          encoding = "",
          file = x$response$disk,
          string = body
        )
      ),
      recorded_at = paste0(format(Sys.time(), tz = "GMT"), " GMT"),
      recorded_with = pkg_versions()
    )
  )
  if (bytes) {
    str_index <- which(grepl("string", names(res[[1]]$response$body)))
    names(res[[1]]$response$body)[str_index] <- "base64_string"
  }
  return(res)
}

# param x: a list with "request" and "response" slots
# param file: a file path
# param bytes: logical, whether to preserve exact bytes or not
write_interactions <- function(x, file, bytes) {
  z <- prep_interaction(x, file, bytes)
  z <- headers_remove(z)
  z <- query_params_remove(z)
  tmp <- yaml::as.yaml(z)
  tmp <- sensitive_remove(tmp)
  cat(tmp, file = file, append = TRUE)
}

write_interactions_json <- function(x, file, bytes) {
  z <- prep_interaction(x, file, bytes)
  z <- headers_remove(z)
  z <- query_params_remove(z)
  # combine with existing data on same file, if any
  on_disk <- invisible(tryCatch(jsonlite::fromJSON(file, FALSE),
    error = function(e) e))
  if (!inherits(on_disk, "error") && is.list(on_disk)) {
    z <- c(on_disk$http_interactions, z)
  }
  tmp <- jsonlite::toJSON(
    list(http_interactions = z), auto_unbox = TRUE, pretty = vcr_c$json_pretty)
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

encoding_guess <- function(x, bytes = FALSE, force_guess = FALSE) {
  if (bytes && !force_guess) return("ASCII-8BIT")
  enc <- try_encoding(x)
  if (enc == "unknown") {
    message("encoding couldn't be detected; assuming UTF-8")
  }
  return("UTF-8")
}
