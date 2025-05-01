request_matches <- function(
  req1,
  req2,
  match_requests_on = c("method", "uri")
) {
  match_1 <- make_comparison(match_requests_on, req1)
  match_2 <- make_comparison(match_requests_on, req2)
  compare <- waldo::compare(
    match_1,
    match_2,
    x_arg = "matching",
    y_arg = "recorded"
  )

  if (length(compare) == 0) {
    vcr_log_sprintf("  match: %s", request_summary(req1))
    TRUE
  } else {
    vcr_log_sprintf("  no match: %s", request_summary(req1))
    lines <- strsplit(paste0(compare, collapse = "\n"), "\n")[[1]]
    lapply(lines, \(line) vcr_log_sprintf("  %s", line))
    FALSE
  }
}

make_comparison <- function(matches, req) {
  uri <- normalize_uri(req$uri, drop_port = "uri" %in% matches)
  needs_uri <- "uri" %in% matches || "uri_with_port" %in% matches

  compact(list(
    method = if ("method" %in% matches) req$method,
    body = if ("body" %in% matches) normalize_body(req$body),
    headers = if ("headers" %in% matches) req$headers,
    uri = if (needs_uri) uri,
    host = if ("host" %in% matches) uri$host,
    path = if ("path" %in% matches) uri$path,
    query = if ("query" %in% matches) uri$params
  ))
}

normalize_body <- function(x) {
  if (!is.list(body)) {
    return(body)
  }

  is_file <- vapply(body, \(f) inherits(f, "form_file"), logical(1))
  body[is_file] <- lapply(body[is_file], unclass)
  body
}

normalize_uri <- function(x, drop_port = TRUE) {
  if (is.null(x)) {
    return(NULL)
  }

  x <- decode_uri(x)

  parsed <- curl::curl_parse_url(x)
  parsed$url <- NULL
  parsed$query <- NULL

  parsed$path <- sub("/$", "", parsed$path)

  if (drop_port) {
    parsed$port <- NULL
  }

  if (length(parsed$params) == 0) {
    parsed$params <- NULL
  }
  compact(parsed)
}
