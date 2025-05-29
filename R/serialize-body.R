encode_body <- function(body, file = FALSE, preserve_bytes = FALSE) {
  if (isTRUE(file)) {
    list(on_disk = body)
  } else {
    if (is.null(body)) {
      NULL
    } else if (is.list(body)) {
      list(fields = body)
    } else if (is_string(body) && !preserve_bytes) {
      list(string = encode_sensitive(body))
    } else if (is.raw(body) || preserve_bytes) {
      data <- memCompress(body, type = "gzip")
      list(raw_gzip = to_base64(data))
    } else {
      cli::cli_abort("Unsupported body type", .internal = TRUE)
    }
  }
}

decode_body <- function(body, preserve_bytes = FALSE) {
  if (preserve_bytes && has_name(body, "string") && is_base64(body$string)) {
    name <- current_cassette()$name
    cli::cli_warn(
      "{.str {name}} cassette uses outdated encoding. Please rerecord it."
    )
    body$base64_string <- body$string
    body$string <- NULL
  }

  if (has_name(body, "on_disk")) {
    list(data = body$on_disk, on_disk = TRUE)
  } else if (isTRUE(body$file)) {
    # In v1, on_disk bodies were recorded with `file = TRUE` and
    # a `string` body giving the path.
    list(data = body$string, on_disk = TRUE)
  } else if (has_name(body, "string")) {
    if (isFALSE(body$string) || identical(body$string, "")) {
      # v1 encoding
      list(data = NULL, on_disk = FALSE)
    } else {
      list(data = decode_sensitive(body$string), on_disk = FALSE)
    }
  } else if (length(body) == 0) {
    list(data = NULL, on_disk = FALSE)
  } else if (has_name(body, "fields")) {
    list(data = body$fields, on_disk = FALSE)
  } else if (has_name(body, "raw_gzip")) {
    data <- from_base64(body$raw_gzip)
    data <- memDecompress(data, type = "gzip")
    list(data = data, on_disk = FALSE)
  } else if (has_name(body, "base64_string")) {
    # In v1, raw bodies were recorded in `base64_string`
    list(data = from_base64(body$base64_string), on_disk = FALSE)
  } else {
    cli::cli_abort("Unsupported body type", .internal = TRUE)
  }
}

# Helpers --------------------------------------------------------------------
from_base64 <- function(x) {
  if (is.character(x)) {
    x <- gsub("[\r\n]", "", x)
  }
  jsonlite::base64_dec(x)
}

# https://datatracker.ietf.org/doc/html/rfc4648#section-4
is_base64 <- function(x) {
  if (!is_string(x) || is.na(x) || !nzchar(x)) {
    return(FALSE)
  }

  # Remove newlines that might be present in formatted base64
  x <- gsub("[\r\n]", "", x)

  # Check if string length is divisible by 4 (base64 requirement)
  if (nchar(x) %% 4 != 0) {
    return(FALSE)
  }

  # Check if string only contains valid base64 characters
  # Valid chars: A-Z, a-z, 0-9, +, /, and = (for padding)
  valid_chars <- grepl("^[A-Za-z0-9+/=]+$", x)
  if (!valid_chars) {
    return(FALSE)
  }

  # Check if padding is valid (if present)
  # Can only be at the end and max 2 '=' characters
  padding_match <- grepl("^[A-Za-z0-9+/]+={0,2}$", x)
  if (!padding_match) {
    return(FALSE)
  }

  # Try to decode and check if it returns raw bytes
  tryCatch(
    {
      decoded <- jsonlite::base64_dec(x)
      is.raw(decoded)
    },
    error = function(e) {
      FALSE
    }
  )
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
