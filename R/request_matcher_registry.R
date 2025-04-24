request_matches <- function(req1, req2, match_requests_on) {
  for (matcher in match_requests_on) {
    match <- request_matches_one(matcher, req1, req2)
    vcr_log_sprintf(
      "    %s %s: current request [%s] vs [%s]",
      matcher,
      if (match) "matched" else "did not match",
      request_summary(req1, match_requests_on),
      request_summary(req2, match_requests_on)
    )

    if (!match) {
      return(FALSE)
    }
  }

  TRUE
}

request_matches_one <- function(type, req1, req2) {
  switch(
    type,
    method = req1$method == req2$method,
    uri = identical(
      url_without_port(encode_uri(req1$uri, flip = TRUE)),
      url_without_port(req2$uri)
    ),
    uri_with_port = identical(
      curl::curl_unescape(encode_uri(req1$uri, flip = TRUE)),
      curl::curl_unescape(req2$uri)
    ),
    body = identical(req1$body, req2$body),
    headers = identical(req1$headers, req2$headers),
    host = identical(url_host(req1$uri), url_host(req2$uri)),
    path = identical(url_path(req1$uri), url_path(req2$uri)),
    query = identical(url_query(req1$uri), url_query(req2$uri)),
    cli::cli_abort("Unsupported request matcher {.str {type}}.")
  )
}

url_path <- function(x) {
  sub("/$", "", curl::curl_parse_url(x)$path)
}
url_query <- function(x) {
  curl::curl_parse_url(x)$query
}
url_host <- function(x) {
  curl::curl_parse_url(x)$host
}
url_without_port <- function(x) {
  url <- curl::curl_parse_url(x)
  url$port <- NULL
  url$url <- NULL
  url
}
