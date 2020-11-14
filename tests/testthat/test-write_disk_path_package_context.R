context("write_disk_path: package context")

test_that("use_cassette w/ request that writes to disk: crul", {
  skip_on_cran()

  dir <- file.path(tempdir(), "rabbit")
  invisible(make_pkg(dir))
  res <- use_vcr(dir, verbose = FALSE)
  dir.create(file.path(dir, "tests/fixtures"), recursive = TRUE)
  dir.create(file.path(dir, "tests/files"), recursive = TRUE)
  strg <- 'ffff <- function() {
  f <- tempfile(fileext = ".json")
  con <- crul::HttpClient$new("https://httpbin.org")
  con$get("get", query = list(apples = 56), disk = f)
}\n\ntest_that("ffff works", {
  vcr::use_cassette("ffff_testing", {
    x <- ffff()
    expect_is(x, "HttpResponse")
    expect_match(x$url, "apples")
  })
})'
  cat(strg, file = file.path(dir, "tests/testthat/test-ffff.R"))
  z <- 'library("vcr")
invisible(vcr::vcr_configure(
  dir = "../fixtures",
  write_disk_path = "../files"
))
vcr::check_cassette_names()'
  cat(z, file = file.path(dir, "tests/testthat/setup-rabbit.R"))

  file_string <- '{
  "args": {
    "apples": "56"
  },
  "headers": {
    "Accept": "application/json, text/xml, application/xml, */*",
    "Accept-Encoding": "gzip, deflate",
    "Host": "httpbin.org",
    "User-Agent": "libcurl/7.64.1 r-curl/4.3 crul/0.9.0",
    "X-Amzn-Trace-Id": "Root=1-5e78ddc3-6bdd2bd4ef4d3082831b10ea"
  },
  "origin": "24.21.229.59",
  "url": "https://httpbin.org/get?apples=56"
}'
  cat(file_string, file = file.path(dir, "tests/files/file3aa4401aca64.json"))

  fixtures1 <- "http_interactions:
- request:
    method: get
    uri: https://httpbin.org/get?apples=56
    body:
      encoding: ''
      string: ''
    headers:
      User-Agent: libcurl/7.64.1 r-curl/4.3 crul/0.9.0
      Accept-Encoding: gzip, deflate
      Accept: application/json, text/xml, application/xml, */*
  response:
    status:
      status_code: '200'
      message: OK
      explanation: Request fulfilled, document follows
    headers:
      status: 'HTTP/2 200 '
      date: Mon, 23 Mar 2020 16:03:15 GMT
      content-type: application/json
      content-length: '397'
      server: gunicorn/19.9.0
      access-control-allow-origin: '*'
      access-control-allow-credentials: 'true'
    body:
      encoding: UTF-8
      file: yes
      string: ../files/file3aa4401aca64.json
  recorded_at: 2020-03-23 16:03:15 GMT
  recorded_with: vcr/0.5.0.92, webmockr/0.6.0.92
"
  cat(fixtures1, file = file.path(dir, "tests/fixtures/ffff_testing.yml"))

  unlink(file.path(dir, "tests/testthat/test-vcr_example.R"))

  og <- getwd()
  setwd(dir)
  on.exit(setwd(og))

  mm <- testthat::test_dir("tests/testthat", reporter = testthat::ListReporter$new())
  expect_equal(capture.output(mm[[1]]$results[[1]]), "As expected ")
  expect_equal(capture.output(mm[[1]]$results[[2]]), "As expected ")

  # cleanup
  unlink(dir, TRUE, TRUE)
})

# reset configuration
vcr_configure_reset()
