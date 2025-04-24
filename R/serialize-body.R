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

decode_body <- function(body, preserve_bytes = FALSE) {
  if (has_name(body, "string") && preserve_bytes) {
    warning("re-record cassettes using 'preserve_exact_body_bytes = TRUE'")
  } else if (has_name(body, "base64_string")) {
    body$base64_string <- from_base64(body$base64_string)
  }
  body
}

# Helpers --------------------------------------------------------------------

from_base64 <- function(x) {
  x <- gsub("[\r\n]", "", x)
  jsonlite::base64_dec(x)
}

to_base64 <- function(x) {
  x <- jsonlite::base64_enc(x)

  # Split into lines of 80 characters
  chunk_size <- 80
  length <- nchar(x)
  if (length < chunk_size) {
    return(x)
  }

  num_chunks <- ceiling(length / 80)
  start_positions <- seq(1, by = 80, length.out = num_chunks)
  end_positions <- pmin(start_positions + chunk_size - 1, length)

  lines <- substring(x, start_positions, end_positions)
  paste0(lines, collapse = "\n")
}
