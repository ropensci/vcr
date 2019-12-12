context("httr: handling images")

tmpdir <- tempdir()
vcr_configure(dir = tmpdir)

test_that("use_cassette w/ binary files on disk with image: crul", {
  skip_on_cran()
  library(httr)

  url <- "https://httpbin.org/image/jpeg"

  # works on 1st request - doing a real http request
  use_cassette("test_write_httr_binary_img", {
    out <- httr::GET(url)
  })

  expect_is(out, "response")
  expect_is(out$content, "raw")
  expect_is(httr::content(out), "array")

  # works on 2nd request - using cassette
  use_cassette("test_write_httr_binary_img", {
    out2 <- httr::GET(url)
  })
  expect_is(out2, "response")
  expect_is(out2$content, "raw")
  expect_is(httr::content(out2), "array")

  expect_identical(httr::content(out), httr::content(out2))
})

# cleanup
files <- c("test_write_httr_binary_img.yml")
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
