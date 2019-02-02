write_cassette <- function(cassette, result){
  file <- get_cassette_data_paths()[cassette$name][[1]]
  write_yaml(result, file)
}

write_yaml <- function(x, file, bytes) {
  write_header(file)
  lapply(x, write_interactions, file = file, bytes = bytes)
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

# param x: a list with "request" and "response" slots
# param file: a file path
# param bytes: logical, whether to preserve exact bytes or not
# NOTE: changed fxn to write body separately to avoid yaml crashes
write_interactions <- function(x, file, bytes) {
  # check types
  assert(x, c("list", "HTTPInteraction"))
  assert(file, "character")

  body <- if (bytes) {
    base64enc::base64encode(charToRaw(get_body(x$response$body)))
  } else {
    get_body(x$response$body)
  }

  # count characters (the count not used anymore,
  # not used as a shorthand to see if it will fail with yaml which malloc fails)
  body_nchar <- tryCatch(nchar(body), error = function(e) e)
  # if errors, may be an encoding issue, coerce to utf-8 first
  # then shouldn't fail with yaml pkg
  body <- enc2utf8(body)
  # if (inherits(body_nchar, "error")) {
  #   body_nchar <- nchar(body)
  # }

  tmp <- yaml::as.yaml(
    list(
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
            encoding = encoding_guess(x$response$body, bytes),
            # handle large bodies
            string = body
            # string = if (body_nchar < 1000000L) {
            #   body
            # } else {
            #   "vcr_replace_me"
            # }
          )
        ),
        recorded_at = paste0(format(Sys.time(), tz = "GMT"), " GMT"),
        recorded_with = pkg_versions()
      )
    )
  )

  # handle large bodies
  # if (body_nchar >= 1000000L) tmp <- sub("vcr_replace_me", body, tmp)
  # if (body_nchar >= 1000000L) tmp <- sub("vcr_replace_me", yaml::as.yaml(body), tmp)

  # filter_sensitive_data replacement
  # FIXME: eventually move to higher level so that this happens
  #  regardless of serializer
  tmp <- sensitive_remove(tmp)

  # write to disk/cassette
  cat(tmp, file = file, append = TRUE)
}

pkg_versions <- function() {
  paste(
    paste0("vcr/", utils::packageVersion("vcr")),
    paste0("webmockr/", utils::packageVersion("webmockr")),
    sep = ", "
  )
}

forwrite <- function(name, x, file){
  cf(name, file)
  for (i in seq_along(x)) {
    cf(sprintf("  %s: '%s'", names(x[i]), x[[i]]), file)
  }
}

cf <- function(x, f){
  cat(paste0("   ", x), sep = "\n", file = f, append = TRUE)
}

get_body <- function(x) {
  if (is.null(x)) '' else x
}

strex <- function(string, pattern) {
  regmatches(string, regexpr(pattern, string))
}

encoding_guess <- function(x, bytes = FALSE, force_guess = FALSE) {
  if (bytes && !force_guess) return("ASCII-8BIT")
  enc <- Encoding(x)
  if (enc == "unknown") {
    message("encoding couldn't be detected; assuming UTF-8")
  }
  return("UTF-8")
}
