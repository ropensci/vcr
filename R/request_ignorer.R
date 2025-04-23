should_be_ignored <- function(request) {
  host <- curl::curl_parse_url(request$uri)$host

  ignored_hosts <- vcr_c$ignore_hosts
  if (isTRUE(vcr_c$ignore_localhost)) {
    ignored_hosts <- union(ignored_hosts, LOCALHOST_ALIASES)
  }

  host %in% ignored_hosts
}

LOCALHOST_ALIASES <- c('localhost', '127.0.0.1', '0.0.0.0')
