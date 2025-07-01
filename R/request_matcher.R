request_matches <- function(
  req1,
  req2,
  match_requests_on = "default",
  i = 1
) {
  if (identical(match_requests_on, "default")) {
    match_requests_on <- default_matcher(req1)
  }

  match_1 <- make_comparison(match_requests_on, req1)
  match_2 <- make_comparison(match_requests_on, req2)
  compare <- waldo::compare(
    match_1,
    match_2,
    x_arg = "matching",
    y_arg = "recorded"
  )

  if (length(compare) == 0) {
    vcr_log_sprintf("    Request %i: MATCH", i)
    TRUE
  } else {
    vcr_log_sprintf("    Request %i: NO MATCH", i)
    lines <- strsplit(paste0(compare, collapse = "\n"), "\n")[[1]]
    lapply(lines, \(line) vcr_log_sprintf("      %s", line))
    FALSE
  }
}

default_matcher <- function(req) {
  if (is.null(req$body)) {
    body <- NULL
  } else {
    body <- if (is_json(req$body)) "body_json" else "body"
  }
  c("method", "uri", body)
}

make_comparison <- function(matches, req) {
  uri <- normalize_uri(req$uri, drop_port = "uri" %in% matches)
  needs_uri <- "uri" %in% matches || "uri_with_port" %in% matches

  compact(list(
    method = if ("method" %in% matches) req$method,
    body = if ("body" %in% matches) normalize_body(req$body),
    body = if ("body_json" %in% matches) try_json(req$body),
    headers = if ("headers" %in% matches) {
      encode_headers(req$headers, "request")
    },
    uri = if (needs_uri) uri,
    host = if ("host" %in% matches) uri$host,
    path = if ("path" %in% matches) uri$path,
    query = if ("query" %in% matches) uri$params
  ))
}

try_json <- function(x) {
  tryCatch(jsonlite::parse_json(x), error = function(e) x)
}
is_json <- function(x) {
  tryCatch(
    {
      jsonlite::parse_json(x)
      TRUE
    },
    error = function(e) FALSE
  )
}

normalize_body <- function(body) {
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
    parsed$params <- set_names(list())
  } else {
    parsed$params <- as.list(parsed$params)
  }
  compact(parsed)
}
