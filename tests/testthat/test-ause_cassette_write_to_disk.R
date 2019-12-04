context("use_cassette: write to disk")

tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("use_cassette w/ request that writes to disk: crul", {
  skip_on_cran()

  library(crul)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make a request
  use_cassette("test_write_to_disk", {
    out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
  })

  expect_is(out, "HttpResponse")
  expect_is(out$content, "character")
  expect_match(out$content, "\\.json")
  expect_is(out$parse(), "character")
  expect_match(out$parse(), "httpbin")

  # works on 2nd request
  use_cassette("test_write_to_disk", {
    out2 <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
  })
  expect_is(out2, "HttpResponse")
  expect_is(out2$content, "character")
  expect_match(out2$content, "\\.json")
  expect_is(out2$parse(), "character")
  expect_match(out2$parse(), "httpbin")

  expect_equal(out$parse(), out2$parse())
})

test_that("use_cassette w/ request that writes to disk: httr", {
  skip_on_cran()

  library(httr)
  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make a request
  use_cassette("test_write_to_disk_httr", {
    out <- GET("https://httpbin.org/get", write_disk(f, TRUE))
  })

  expect_is(out, "response")
  expect_is(out$content, "path")
  expect_match(out$content, "\\.json")

  # works on 2nd request
  use_cassette("test_write_to_disk_httr", {
    out2 <- GET("https://httpbin.org/get", write_disk(f, TRUE))
  })
  expect_is(out2, "response")
  expect_is(out2$content, "path")
  expect_match(out2$content, "\\.json")

  expect_equal(httr::content(out), httr::content(out2))
})

# cleanup
files <- c("test_write_to_disk.yml", "test_write_to_disk_httr.yml")
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
