encode_body <- function(body, file, preserve_bytes = FALSE) {
  if (is.raw(body) || preserve_bytes) {
    compact(list(
      base64_string = to_base64(body),
      file = file
    ))
  } else {
    compact(list(
      string = sensitive_remove(body),
      file = file
    ))
  }
}

decode_body <- function(body, preserve_bytes = FALSE) {
  if (has_name(body, "string") && preserve_bytes) {
    warning("re-record cassettes using 'preserve_exact_body_bytes = TRUE'")
  }

  if (has_name(body, "base64_string")) {
    data <- from_base64(body$base64_string)
  } else {
    data <- body$string
  }

  list(data = data, file = body$file)
}

# Helpers --------------------------------------------------------------------

from_base64 <- function(x) {
  x <- gsub("[\r\n]", "", x)
  jsonlite::base64_dec(x)
}

to_base64 <- function(x) {
  if (is.null(x)) {
    return(NULL)
  }

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
