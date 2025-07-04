# Interactions -----------------------------------------------------------------

encode_interactions <- function(
  interactions,
  preserve_bytes = FALSE,
  matchers = "default"
) {
  interactions <- lapply(
    interactions,
    encode_interaction,
    preserve_bytes = preserve_bytes,
    matchers = matchers
  )
  list(
    http_interactions = interactions,
    # Include VCR so linguist recognises as a generated file
    # https://github.com/github-linguist/linguist/blob/main/lib/linguist/generated.rb#L564-L569
    recorded_with = paste0("VCR-", pkg_versions())
  )
}

decode_interactions <- function(
  interactions,
  preserve_bytes = FALSE
) {
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

encode_interaction <- function(
  interaction,
  preserve_bytes = FALSE,
  matchers = "default"
) {
  list(
    request = encode_request(interaction$request, preserve_bytes, matchers),
    response = encode_response(interaction$response, preserve_bytes),
    recorded_at = format_time(interaction$recorded_at)
  )
}

encode_request <- function(
  request,
  preserve_bytes = FALSE,
  matchers = "default"
) {
  compact(list(
    method = request$method,
    uri = encode_uri(request$uri),
    body = if (any(c("body", "body_json", "default") %in% matchers)) {
      encode_body(request$body, NULL, preserve_bytes)
    },
    headers = if ("headers" %in% matchers) {
      encode_headers(request$headers, "request")
    }
  ))
}

encode_response <- function(response, preserve_bytes = FALSE) {
  if (is.null(response)) {
    return(NULL)
  }

  compact(list(
    status = response$status,
    headers = encode_headers(response$headers, "response"),
    body = encode_body(response$body, response$disk, preserve_bytes)
  ))
}

decode_interaction <- function(interaction, preserve_bytes = FALSE) {
  request <- interaction$request
  response <- interaction$response

  request_body <- decode_body(request$body, preserve_bytes = preserve_bytes)
  response_body <- decode_body(response$body, preserve_bytes = preserve_bytes)

  # status codes were previously written as a list
  status <- response$status
  if (is.list(status)) {
    status <- as.numeric(status$status_code)
  }

  vcr_interaction(
    vcr_request(
      method = request$method,
      uri = decode_uri(request$uri),
      body = request_body$data,
      headers = decode_headers(request$headers)
    ),
    vcr_response(
      status = status,
      headers = decode_headers(response$headers),
      body = response_body$data,
      disk = response_body$on_disk
    ),
    as.POSIXct(interaction$recorded_at, tz = "UTC")
  )
}
