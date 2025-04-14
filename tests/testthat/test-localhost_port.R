# orginally via https://github.com/ropensci/vcr/issues/264

test_that("testing against localhost port works", {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/status/404")

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
  expect_equal(file$http_interactions[[1]]$request$uri, url)
})
