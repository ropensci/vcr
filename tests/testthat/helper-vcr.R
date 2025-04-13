local_vcr_configure <- function(..., .frame = parent.frame()) {
  old <- vcr_configure(...)
  withr::defer(vcr_config_set(old), envir = .frame)
}

desc_text <- "Package: %s
Title: Does A Thing
Description: Does a thing.
Version: 0.0.1
Author: Jane Doe
Maintainer: Jane Doe <jane@doe.com>
License: MIT + file LICENSE
LazyData: true
RoxygenNote: 6.1.1
Suggests:
    testthat\n"

make_pkg <- function(frame = parent.frame()) {
  dir <- withr::local_tempdir(.local_envir = frame)

  dir.create(file.path(dir, "man"), recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  cat(sprintf(desc_text, basename(dir)), file = file.path(dir, "DESCRIPTION"))

  dir
}

has_port <- function(port) crul::ok(paste0('http://localhost:', port))

skip_if_localhost_8000_gone <- function() {
  if (has_port(8000)) return()
  testthat::skip("port 8000 not available")
}

recorded_at <- function(x) {
  yaml::yaml.load_file(x$manfile)$http_interactions[[1]]$recorded_at
}

extract_vcr_config_args <- function(rdfile) {
  stopifnot(file.exists(rdfile))

  rdtext <- paste0(readLines(rdfile), collapse = "")
  rdhits <- gregexpr("item \\\\code\\{([a-z_]+)\\}", rdtext, perl = TRUE)[[1]]

  substring(
    rdtext,
    attr(rdhits, "capture.start"),
    attr(rdhits, "capture.start") + attr(rdhits, "capture.length") - 1
  )
}

check_url <- function(x, ...) {
  suppressWarnings(suppressMessages(crul::ok(x, ...)))
}

hb <- function(x = NULL) {
  server <- getOption("vcr::httpbin_local_server")
  if (is.null(server)) {
    app <- webfakes::httpbin_app()
    server <- webfakes::new_app_process(app)
    options(`vcr::httpbin_local_server` = server)
  }

  server$url(x)
}

hb_remote <- function(x = NULL) {
  base_url <- getOption("vcr::httpbin_server")
  if (is.null(base_url)) {
    base_url <- find_httpbin_server()
    options(`vcr::httpbin_server` = base_url)
  }

  if (is.null(x)) {
    base_url
  } else {
    paste0(base_url, x)
  }
}

find_httpbin_server <- function() {
  urls <- c(
    "https://hb.cran.dev",
    "https://hb.opencpu.org",
    "https://nghttp2.org/httpbin"
  )
  h <- curl::new_handle(timeout = 10, failonerror = FALSE)

  for (i in seq_along(urls)) {
    url <- urls[[i]]
    out <- curl::curl_fetch_memory(url, handle = h)
    if (out$status_code == 200) {
      cat(paste0("using base url for tests: ", url), sep = "\n")
      return(url)
    }
  }
  stop("all httpbin servers down")
}

read_cassette <- function(name) {
  yaml::yaml.load_file(file.path(vcr_c$dir, name))
}
