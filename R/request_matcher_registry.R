request_matches <- function(req1, req2, match_requests_on) {
  for (matcher in match_requests_on) {
    match <- switch(
      matcher,
      method = r1$method == r2$method,
      uri = identical(
        curl::curl_unescape(query_params_remove_str(r1$uri)),
        curl::curl_unescape(r2$uri)
      ),
      body = identical(as.character(r1$body), as.character(r2$body)),
      headers = identical(r1$headers, r2$headers),
      host = identical(r1$host, r2$host),
      path = identical(sub("/$", "", r1$path), sub("/$", "", r2$path)),
      query = identical(
        curl::curl_unescape(r1$query),
        curl::curl_unescape(r2$query)
      ),
      cli::cli_abort("Unsupported request matcher {.str {on}}.")
    )

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
