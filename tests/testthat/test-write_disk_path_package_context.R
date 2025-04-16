test_that("use_cassette w/ request that writes to disk: crul", {
  skip_on_cran()
  local_vcr_configure()

  dir <- make_pkg()
  res <- use_vcr(dir, verbose = FALSE)
  dir_create(file.path(dir, "tests/fixtures"))
  dir_create(file.path(dir, "tests/files"))
  strg <- 'ffff <- function() {
  f <- withr::local_tempfile(fileext = ".json")
  con <- crul::HttpClient$new("https://hb.opencpu.org")
  con$get("get", query = list(apples = 56), disk = f)
}\n\ntest_that("ffff works", {
  use_cassette("ffff_testing", {
    x <- ffff()
    expect_s3_class(x, "HttpResponse")
    expect_match(x$url, "apples")
  })
})'
  cat(strg, file = file.path(dir, "tests/testthat/test-ffff.R"))
  z <- '
invisible(vcr_configure(
  dir = "../fixtures",
  write_disk_path = "../files"
))
vcr::check_cassette_names()'
  cat(z, file = file.path(dir, "tests/testthat/helper-rabbit.R"))

  file_string <- '{
  "args": {
    "apples": "56"
  },
  "headers": {
    "Accept": "application/json, text/xml, application/xml, */*",
    "Accept-Encoding": "gzip, deflate",
    "Host": "hb.opencpu.org",
    "User-Agent": "libcurl/7.64.1 r-curl/4.3 crul/0.9.0",
    "X-Amzn-Trace-Id": "Root=1-5e78ddc3-6bdd2bd4ef4d3082831b10ea"
  },
  "origin": "24.21.229.59",
  "url": "https://hb.opencpu.org/get?apples=56"
}'
  cat(file_string, file = file.path(dir, "tests/files/file3aa4401aca64.json"))

  fixtures1 <- "http_interactions:
- request:
    method: get
    uri: https://hb.opencpu.org/get?apples=56
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

  withr::local_dir(dir)
  mm <- testthat::test_dir(
    "tests/testthat",
    reporter = testthat::ListReporter$new()
  )
  expect_equal(capture.output(mm[[1]]$results[[1]])[2], "As expected")
  expect_equal(capture.output(mm[[1]]$results[[2]])[2], "As expected")
})
