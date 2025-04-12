tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("use_cassette w/ binary files on disk: crul", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  ## url
  url <- "https://dods.ndbc.noaa.gov/thredds/fileServer/data/cwind/41001/41001c1997.nc"
  skip_if(!check_url(url, timeout_ms = 5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), "41001c1990.nc")
  ## make a request
  use_cassette(
    "test_write_to_disk_binary",
    out <- crul::HttpClient$new(url)$get(disk = f)
  )

  expect_s3_class(out, "HttpResponse")
  expect_type(out$content, "character")
  expect_match(out$content, "\\.nc")
  expect_type(out$parse(), "raw")

  # works on 2nd request
  use_cassette("test_write_to_disk_binary", {
    out2 <- crul::HttpClient$new(url)$get(disk = f)
  })
  expect_s3_class(out2, "HttpResponse")
  expect_type(out2$content, "character")
  expect_match(out2$content, "\\.nc")
  expect_type(out2$parse(), "raw")

  expect_equal(out$parse(), out2$parse())
})

test_that("use_cassette w/ binary files on disk with image: crul", {
  skip_on_cran()

  ## url
  # url <- "https://github.com/sckott/rforcats/raw/gh-pages/assets/img/250.jpeg"
  url <- "https://raw.githubusercontent.com/sckott/rforcats/gh-pages/assets/img/250.jpeg"
  # ...github.com/ redirects to
  # "https://raw.githubusercontent.com/sckott/rforcats/gh-pages/assets/img/250.jpeg"
  # thus breaking the test
  skip_if(!check_url(url, timeout_ms = 5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), basename(url))
  ## make a request
  use_cassette(
    "test_write_to_disk_binary_img",
    out <- crul::HttpClient$new(url)$get(disk = f)
  )

  expect_s3_class(out, "HttpResponse")
  expect_type(out$content, "character")
  expect_match(out$content, "\\.jpeg")
  expect_type(out$parse(), "raw")

  # works on 2nd request
  use_cassette(
    "test_write_to_disk_binary_img",
    out2 <- crul::HttpClient$new(url)$get(disk = f)
  )
  expect_s3_class(out2, "HttpResponse")
  expect_type(out2$content, "character")
  expect_match(out2$content, "\\.jpeg")
  expect_type(out2$parse(), "raw")

  expect_equal(out$parse(), out2$parse())

  # a function wrapping crul
  ## example where user doesn't know/specify the path
  ## easy enough to move the file
  foo_bar <- function() {
    # url <- "https://github.com/sckott/rforcats/raw/gh-pages/assets/img/250.jpeg"
    url <- "https://raw.githubusercontent.com/sckott/rforcats/gh-pages/assets/img/250.jpeg"
    f <- file.path(tempdir(), basename(url))
    crul::HttpClient$new(url)$get(disk = f)
  }

  use_cassette(
    "test_write_to_disk_binary_img_fxn",
    out <- foo_bar()
  )
  use_cassette(
    "test_write_to_disk_binary_img_fxn",
    out2 <- foo_bar()
  )

  expect_type(out$content, "character")
  expect_type(out$parse(), "raw")
  expect_type(out2$content, "character")
  expect_type(out2$parse(), "raw")
  expect_equal(out$parse(), out2$parse())
})


test_that("use_cassette w/ binary files on disk: httr", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  ## url
  url <- "https://dods.ndbc.noaa.gov/thredds/fileServer/data/cwind/41001/41001c1997.nc"
  skip_if(!check_url(url, timeout_ms = 5000L), sprintf("url not up (%s)", url))
  ## make a temp file
  f <- file.path(tempdir(), "41001c1990.nc")
  ## make a request
  use_cassette(
    "test_write_to_disk_binary_httr",
    out <- httr::GET(url, httr::write_disk(f, TRUE))
  )

  expect_s3_class(out, "response")
  expect_s3_class(out$content, "path")
  expect_match(out$content, "\\.nc")

  # works on 2nd request
  use_cassette(
    "test_write_to_disk_binary_httr",
    out2 <- httr::GET(url, httr::write_disk(f, TRUE))
  )
  expect_s3_class(out2, "response")
  expect_s3_class(out2$content, "path")
  expect_match(out2$content, "\\.nc")

  expect_equal(httr::content(out), httr::content(out2))
})
