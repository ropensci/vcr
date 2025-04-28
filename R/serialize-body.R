encode_body <- function(body, file, preserve_bytes = FALSE) {
  if (isTRUE(file)) {
    list(on_disk = body)
  } else {
    if (is.null(body)) {
      set_names(list())
    } else if (is.list(body)) {
      list(fields = body)
    } else if (is.raw(body) || preserve_bytes) {
      list(base64_string = to_base64(body))
    } else if (is_string(body)) {
      list(string = encode_sensitive(body))
    } else {
      cli::cli_abort("Unsupported body type", .internal = TRUE)
    }
  }
}

decode_body <- function(body, preserve_bytes = FALSE) {
  if (has_name(body, "string") && preserve_bytes) {
    warning("re-record cassettes using 'preserve_exact_body_bytes = TRUE'")
  }

  if (has_name(body, "fields")) {
    list(data = body$fields, on_disk = FALSE)
  } else if (has_name(body, "on_disk")) {
    list(data = body$on_disk, on_disk = TRUE)
  } else if (has_name(body, "base64_string")) {
    list(data = from_base64(body$base64_string), on_disk = FALSE)
  } else {
    # In v1, on_disk bodies were recorded with `file = TRUE` and
    # a `string` body giving the path.
    list(data = decode_sensitive(body$string), on_disk = FALSE)
  }
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
