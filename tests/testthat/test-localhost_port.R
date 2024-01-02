# orginally via https://github.com/ropensci/vcr/issues/264

tmpdir <- tempdir()
vcr_configure(dir = tmpdir)

test_that("testing against localhost port works", {
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
  path <- file.path(tmpdir, "localhost_port.yml")
  file <- yaml::yaml.load_file(path)
  url <- file$http_interactions[[1]]$request$uri
  expect_is(url, "character")
  expect_match(url, "http://.+:[0-9]+/")
  expect_match(url, as.character(port))
})

# reset configuration
vcr_configure_reset()
