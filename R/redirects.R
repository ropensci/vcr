scode <- function(x) {
  if ("status_code" %in% names(x)) return(x$status_code)
  return(x$status$status_code)
}
last_http_status <- function(cas) {
  z <- last(cas$merged_interactions())
  if (!length(z)) "" else scode(z[[1]]$response)
  # $status$status_code
}
redirects_remaining <- function(cas) {
  stat <- as.character(last_http_status(cas))
  sac$last_http_status_code <- stat
  if (!nzchar(stat)) return(TRUE)
  stat %in% c("301", "302", "303", "307", "308")
}
http_client <- function(x) {
  ifelse(is.list(x$url), "crul", "httr")
}
update_relative <- function(req, path) {
  pkg <- http_client(req)
  next_url <- path
  if (!grepl('://', path, fixed = TRUE)) {
    # server <- sub("^(.*://[^/]+).*", "\\1", url)
    # nexturl <- paste0(server, nexturl)
    tmp <- urltools::url_parse(switch(pkg, crul=req$url$url, httr=req$url))
    tmp$path <- sub("^/", "", path)
    next_url <- urltools::url_compose(tmp)
  }

  if (pkg == "crul") {
    req$url$url <- next_url
  } else {
    req$url <- next_url
  }
  if (pkg == "crul")
    curl::handle_setopt(req$url$handle, followlocation = 0L)
  return(req)
}
