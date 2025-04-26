# Interactions -----------------------------------------------------------------

encode_interactions <- function(x, preserve_bytes = FALSE) {
  interactions <- lapply(x, encode_interaction, preserve_bytes)
  list(
    http_interactions = interactions,
    recorded_with = pkg_versions()
  )
}

decode_interactions <- function(x, preserve_bytes = FALSE) {
  if (is.null(x)) return(list())

  x$http_interactions <- lapply(
    x$http_interactions,
    decode_interaction,
    preserve_bytes = preserve_bytes
  )
  x
}

# Interaction ------------------------------------------------------------------

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

decode_interaction <- function(x, preserve_bytes) {
  request_body <- decode_body(x$request$body, preserve_bytes = preserve_bytes)
  response_body <- decode_body(x$response$body, preserve_bytes = preserve_bytes)

  list(
    request = vcr_request(
      method = x$request$method,
      uri = x$request$uri,
      body = request_body$data,
      headers = x$request$headers
    ),
    response = vcr_response(
      status = x$response$status,
      headers = x$response$headers,
      body = response_body$data,
      disk = response_body$file
    )
  )
}
