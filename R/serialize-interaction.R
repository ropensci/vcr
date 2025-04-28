# Interactions -----------------------------------------------------------------

encode_interactions <- function(interactions, preserve_bytes = FALSE) {
  interactions <- lapply(interactions, encode_interaction, preserve_bytes)
  list(
    http_interactions = interactions,
    recorded_with = pkg_versions()
  )
}

decode_interactions <- function(interactions, preserve_bytes = FALSE) {
  if (is.null(interactions)) {
    return(list())
  }

  interactions$http_interactions <- lapply(
    interactions$http_interactions,
    decode_interaction,
    preserve_bytes = preserve_bytes
  )
  interactions
}

# Interaction ------------------------------------------------------------------

encode_interaction <- function(interaction, preserve_bytes = FALSE) {
  request <- interaction$request
  response <- interaction$response

  request_raw <- preserve_bytes || has_binary_content(request$headers)
  response_raw <- preserve_bytes || has_binary_content(response$headers)

  list(
    request = list(
      method = request$method,
      uri = encode_uri(request$uri),
      body = encode_body(request$body, NULL, request_raw),
      headers = encode_headers(request$headers, "request")
    ),
    response = list(
      status = response$status,
      headers = encode_headers(response$headers, "response"),
      body = encode_body(response$body, response$disk, response_raw)
    ),
    recorded_at = paste0(cur_time(tz = "GMT"), " GMT")
  )
}

decode_interaction <- function(interaction, preserve_bytes) {
  request <- interaction$request
  response <- interaction$response

  request_body <- decode_body(request$body, preserve_bytes = preserve_bytes)
  response_body <- decode_body(response$body, preserve_bytes = preserve_bytes)

  list(
    request = vcr_request(
      method = request$method,
      uri = decode_uri(request$uri),
      body = request_body$data,
      headers = decode_headers(request$headers)
    ),
    response = vcr_response(
      status = response$status,
      headers = decode_headers(response$headers),
      body = response_body$data,
      disk = response_body$file
    )
  )
}


# Helpers --------------------------------------------------------------------

has_binary_content <- function(headers) {
  idx <- match("content-type", tolower(names(headers)))
  if (is.na(idx)) {
    FALSE
  } else {
    is_binary_type(headers[[idx]])
  }
}

is_binary_type <- function(content_type) {
  parsed <- parse_content_type(content_type)
  if (parsed$type == "text") {
    return(FALSE)
  }

  special_cases <- c(
    "application/xml",
    "application/x-www-form-urlencoded",
    "application/json",
    "application/ld+json",
    "multipart/form-data"
  )
  base_type <- paste0(parsed$type, "/", parsed$subtype)
  if (base_type %in% special_cases) {
    return(FALSE)
  }

  TRUE
}

# Copied from httr2::parse_content_type
parse_content_type <- function(x) {
  stopifnot(length(x) == 1)
  regex <- "^(?<type>application|audio|font|example|image|message|model|multipart|text|video)/(?<subtype>(?:(?:vnd|prs|x)\\.)?(?:[^+;])+)(?:\\+(?<suffix>(?:[^;])+))?(?:;(?<parameters>(?:.)+))?$"
  if (!grepl(regex, x, perl = TRUE)) {
    out <- list(
      type = "",
      subtype = "",
      suffix = ""
    )
    return(out)
  }

  match_object <- regexec(regex, x, perl = TRUE)
  match <- regmatches(x, match_object)[[1]]
  list(
    type = match[[2]],
    subtype = match[[3]],
    suffix = if (match[[4]] != "") match[[4]] else ""
  )
}
