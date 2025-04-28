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

encode_interaction <- function(interaction, preserve_bytes) {
  request <- interaction$request
  response <- interaction$response

  list(
    request = list(
      method = request$method,
      uri = encode_uri(request$uri),
      body = encode_body(request$body, NULL, preserve_bytes),
      headers = encode_headers(request$headers, "request")
    ),
    response = list(
      status = response$status,
      headers = encode_headers(response$headers, "response"),
      body = encode_body(response$body, response$disk, preserve_bytes)
    ),
    recorded_at = paste0(cur_time(tz = "GMT"), " GMT")
  )
}

decode_interaction <- function(interaction, preserve_bytes) {
  request <- interaction$request
  response <- interaction$response

  request_body <- decode_body(request$body, preserve_bytes = preserve_bytes)
  response_body <- decode_body(response$body, preserve_bytes = preserve_bytes)

  # status codes were previously written as a list
  status <- response$status
  if (is.list(status)) {
    status <- as.numeric(status$status_code)
  }

  list(
    request = vcr_request(
      method = request$method,
      uri = decode_uri(request$uri),
      body = request_body$data,
      headers = decode_headers(request$headers)
    ),
    response = vcr_response(
      status = status,
      headers = decode_headers(response$headers),
      body = response_body$data,
      disk = response_body$on_disk
    )
  )
}
