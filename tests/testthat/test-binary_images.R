context("handling images: httr")

tmpdir <- tempdir()
vcr_configure(dir = tmpdir)

test_that("use_cassette w/ with images: httr", {
  skip_on_cran()
  skip_if_not_installed("jpeg")
  
  library(httr)
  url <- hb("/image/jpeg")

  ## preserve_exact_body_bytes = FALSE
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
  
  ## preserve_exact_body_bytes = TRUE
  # works on 1st request - doing a real http request
  use_cassette("test_write_httr_binary_img_bytes", {
    res1 <- httr::GET(url)
  }, preserve_exact_body_bytes = TRUE)
  
  expect_is(res1, "response")
  expect_is(res1$content, "raw")
  expect_is(httr::content(res1), "array")
  expect_equal(length(res1$content), 35588)

  use_cassette("test_write_httr_binary_img_bytes", {
    res2 <- httr::GET(url)
  }, preserve_exact_body_bytes = TRUE)
  
  expect_is(res2, "response")
  expect_is(res2$content, "raw")
  expect_is(httr::content(res2), "array")
  expect_equal(length(res2$content), 35588)
})

context("handling images: crul")

test_that("use_cassette w/ with images: crul", {
  skip_on_cran()
  
  library(crul)
  url <- hb("/image/jpeg")

  ## preserve_exact_body_bytes = FALSE
  # works on 1st request - doing a real http request
  use_cassette("test_write_crul_binary_img", {
    out <- crul::HttpClient$new(url)$get()
  })

  expect_is(out, "HttpResponse")
  expect_is(out$content, "raw")

  # works on 2nd request - using cassette
  use_cassette("test_write_crul_binary_img", {
    out2 <- crul::HttpClient$new(url)$get()
  })
  expect_is(out2, "HttpResponse")
  expect_is(out2$content, "raw")

  expect_identical(out$content, out2$content)


  ## preserve_exact_body_bytes = TRUE
  # works on 1st request - doing a real http request
  use_cassette("test_write_crul_binary_img_bytes", {
    res1 <- crul::HttpClient$new(url)$get()
  }, preserve_exact_body_bytes = TRUE)

  expect_is(res1, "HttpResponse")
  expect_is(res1$content, "raw")

  # works on 2nd request - using cassette
  use_cassette("test_write_crul_binary_img_bytes", {
    res2 <- crul::HttpClient$new(url)$get()
  }, preserve_exact_body_bytes = TRUE)
  expect_is(res2, "HttpResponse")
  expect_is(res2$content, "raw")

  expect_identical(res1$content, res2$content)
})

# cleanup
files <- c("test_write_httr_binary_img.yml",
  "test_write_httr_binary_img_bytes.yml",
  "test_write_crul_binary_img.yml",
  "test_write_crul_binary_img_bytes.yml")
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
