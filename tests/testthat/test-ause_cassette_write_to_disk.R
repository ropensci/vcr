context("use_cassette: write to disk")

tmpdir_wdp <- file.path(tempdir(), "write_disk_path")
vcr_configure(dir = tmpdir_wdp)

test_that("fails well if write_disk_path not set", {
  skip_on_cran()

  expect_null(vcr_c$write_disk_path)

  library(crul)
  f <- tempfile(fileext = ".json")
  expect_error(
    use_cassette("write_disk_path_not_set_crul", {
      out <- HttpClient$new("https://httpbin.org/get")$get(disk = f)
    }),
    "write_disk_path must be given"
  )

  library(httr)
  g <- tempfile(fileext = ".json")
  expect_error(
    use_cassette("write_disk_path_not_set_httr", {
      out <- GET("https://httpbin.org/get", write_disk(g, TRUE))
    }),
    "write_disk_path must be given"
  )
})

# cleanup
files <- c("write_disk_path_not_set_crul.yml", "write_disk_path_not_set_httr.yml")
unlink(file.path(vcr_configuration()$dir, files))

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
