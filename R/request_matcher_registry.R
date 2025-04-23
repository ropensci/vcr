request_matches <- function(req1, req2, match_requests_on) {
  for (matcher in match_requests_on) {
    match <- request_matches_one(matcher, req1, req2)
    vcr_log_sprintf(
      "    %s %s: current request [%s] vs [%s]",
      y,
      if (match) "matched" else "did not match",
      request_summary(req, self$request_matchers),
      request_summary(intreq, self$request_matchers)
    )

    if (!match) {
      return(FALSE)
    }
  }

  TRUE
}

request_matches_one <- function(type, req1, req2) {
  match <- switch(
    type,
    method = req1$method == req2$method,
    uri = identical(
      curl::curl_unescape(encode_uri(req1$uri, flip = TRUE)),
      curl::curl_unescape(req2$uri)
    ),
    body = identical(req1$body, req2$body),
    headers = identical(req1$headers, req2$headers),
    host = identical(req1$host, req2$host),
    path = identical(sub("/$", "", req1$path), sub("/$", "", req2$path)),
    query = identical(
      curl::curl_unescape(req1$query),
      curl::curl_unescape(req2$query)
    ),
    cli::cli_abort("Unsupported request matcher {.str {type}}.")
  )
}
