# orginally via https://github.com/ropensci/vcr/issues/264

test_that("testing against localhost port works", {
  local_vcr_configure(dir = withr::local_tempdir())

  # httpbin <- webfakes::local_app_process(webfakes::httpbin_app())
  httpbin <- local_httpbin_app()
  url <- httpbin$url("/status/404")
  port <- httpbin$get_port()

  # real request w/o vcr
  resp <- httr::GET(url)
  expect_s3_class(resp, "response")

  # real request w/ vcr
  use_cassette("localhost_port", {
    resp <- httr::GET(url)
    # check that response object is correct
    expect_s3_class(resp, "response")
  })

  # check that the port is actually in the cassette file
  file <- read_cassette("localhost_port.yml")
  url <- file$http_interactions[[1]]$request$uri
  expect_type(url, "character")
  expect_match(url, "http://.+:[0-9]+/")
  expect_match(url, as.character(port))
})
