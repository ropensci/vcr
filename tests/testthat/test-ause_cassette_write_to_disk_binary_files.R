context("use_cassette: handle binary files on disk")

tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("use_cassette w/ binary files on disk: crul", {
  skip_on_cran()

  library(crul)
  ## url
  url <- "https://dods.ndbc.noaa.gov/thredds/fileServer/data/cwind/41001/41001c1997.nc"
  skip_if(!check_url(url, timeout_ms=5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), "41001c1990.nc")
  ## make a request
  use_cassette("test_write_to_disk_binary", {
    out <- HttpClient$new(url)$get(disk = f)
  })

  expect_is(out, "HttpResponse")
  expect_is(out$content, "character")
  expect_match(out$content, "\\.nc")
  expect_is(out$parse(), "raw")

  # works on 2nd request
  use_cassette("test_write_to_disk_binary", {
    out2 <- HttpClient$new(url)$get(disk = f)
  })
  expect_is(out2, "HttpResponse")
  expect_is(out2$content, "character")
  expect_match(out2$content, "\\.nc")
  expect_is(out2$parse(), "raw")

  expect_equal(out$parse(), out2$parse())
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "test_write_to_disk_binary.yml"))


test_that("use_cassette w/ binary files on disk with image: crul", {
  skip_on_cran()

  library(crul)
  ## url
  url <- "https://github.com/sckott/rforcats/raw/gh-pages/assets/img/250.jpeg"
  skip_if(!check_url(url, timeout_ms=5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), basename(url))
  ## make a request
  use_cassette("test_write_to_disk_binary_img", {
    out <- HttpClient$new(url)$get(disk = f)
  })

  expect_is(out, "HttpResponse")
  expect_is(out$content, "character")
  expect_match(out$content, "\\.jpeg")
  expect_is(out$parse(), "raw")

  # works on 2nd request
  use_cassette("test_write_to_disk_binary_img", {
    out2 <- HttpClient$new(url)$get(disk = f)
  })
  expect_is(out2, "HttpResponse")
  expect_is(out2$content, "character")
  expect_match(out2$content, "\\.jpeg")
  expect_is(out2$parse(), "raw")

  expect_equal(out$parse(), out2$parse())



  # a function wrapping crul
  ## example where user doesn't know/specify the path
  ## easy enough to move the file
  library(crul)
  foo_bar <- function() {
    url <- "https://github.com/sckott/rforcats/raw/gh-pages/assets/img/250.jpeg"
    f <- file.path(tempdir(), basename(url))
    HttpClient$new(url)$get(disk = f)
  }

  use_cassette("test_write_to_disk_binary_img_fxn", {
    out <- foo_bar()
  })
  use_cassette("test_write_to_disk_binary_img_fxn", {
    out2 <- foo_bar()
  })

  expect_is(out$content, "character")
  expect_is(out$parse(), "raw")
  expect_is(out2$content, "character")
  expect_is(out2$parse(), "raw")
  expect_equal(out$parse(), out2$parse())
})


test_that("use_cassette w/ binary files on disk: httr", {
  skip_on_cran()

  library(httr)
  ## url
  url <- "https://dods.ndbc.noaa.gov/thredds/fileServer/data/cwind/41001/41001c1997.nc"
  skip_if(!check_url(url, timeout_ms=5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), "41001c1990.nc")
  ## make a request
  use_cassette("test_write_to_disk_binary_httr", {
    out <- GET(url, write_disk(f, TRUE))
  })

  expect_is(out, "response")
  expect_is(out$content, "path")
  expect_match(out$content, "\\.nc")

  # works on 2nd request
  use_cassette("test_write_to_disk_binary_httr", {
    out2 <- GET(url, write_disk(f, TRUE))
  })
  expect_is(out2, "response")
  expect_is(out2$content, "path")
  expect_match(out2$content, "\\.nc")

  expect_equal(httr::content(out), httr::content(out2))
})

# cleanup
files <- c("test_write_to_disk_binary_httr.yml", "test_write_to_disk_binary_img.yml",
  "test_write_to_disk_binary_img_fxn.yml")
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
