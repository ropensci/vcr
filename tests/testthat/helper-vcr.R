tmpdir <- tempdir()
library(vcr)

# define and restore consistent configuration options for tests
vcr_test_configuration <- function(
  dir = tmpdir,
  write_disk_path = file.path(tmpdir, "files"),
  ...) {
  vcr_configure_reset()
  vcr_configure(dir = dir, write_disk_path = write_disk_path, ...)
}

vcr_test_configuration()

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

make_pkg <- function(dir) {
  if (length(list.files(dir)) > 1)
    stop("dir is not empty")
  dir.create(dir, recursive = TRUE)
  dir.create(file.path(dir, "man"), recursive = TRUE)
  dir.create(file.path(dir, "R"), recursive = TRUE)
  cat(sprintf(desc_text, basename(dir)), file = file.path(dir, "DESCRIPTION"))
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
sw <- function(x) suppressWarnings(x)
sm <- function(x) suppressMessages(x)


# Base url for tests
hb <- function(x = NULL) {
  tryCatch(
    if (is.null(x)) base_url else paste0(base_url, x),
    error = function(e) "https://not.aurl"
  )
}
urls <- c(
  "https://hb.cran.dev",
  "https://hb.opencpu.org",
  "https://nghttp2.org/httpbin"
)
h <- curl::new_handle(timeout = 10, failonerror = FALSE)
base_url <- ""
tryCatch({
  out <- list()
  for (i in seq_along(urls)) {
    out[[i]] <- curl::curl_fetch_memory(urls[i], handle = h)
  }
  codes <- vapply(out, "[[", 1, "status_code")
  if (all(codes != 200)) stop("all httpbin servers down")
  base_url <- urls[codes == 200][1]
  cat(paste0("using base url for tests: ", base_url), sep = "\n")
}, error = function(e) message(e$message))

# httpbin local
local_httpbin_app <- function() {
  check_for_a_pkg("webfakes")
  webfakes::local_app_process(
    webfakes::httpbin_app(),
    .local_envir = testthat::teardown_env()
  )
}
