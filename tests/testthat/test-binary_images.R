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

  expect_s3_class(out, "response")
  expect_type(out$content, "raw")
  expect_equal(dims(httr::content(out)), 3)

  # works on 2nd request - using cassette
  use_cassette("test_write_httr_binary_img", {
    out2 <- httr::GET(url)
  })
  expect_s3_class(out2, "response")
  expect_type(out2$content, "raw")
  expect_equal(dims(httr::content(out2)), 3)

  expect_identical(httr::content(out), httr::content(out2))

  ## preserve_exact_body_bytes = TRUE
  # works on 1st request - doing a real http request
  use_cassette(
    "test_write_httr_binary_img_bytes",
    {
      res1 <- httr::GET(url)
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_s3_class(res1, "response")
  expect_type(res1$content, "raw")
  expect_equal(dims(httr::content(res1)), 3)
  expect_equal(length(res1$content), 35588)

  use_cassette(
    "test_write_httr_binary_img_bytes",
    {
      res2 <- httr::GET(url)
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_s3_class(res2, "response")
  expect_type(res2$content, "raw")
  expect_equal(dims(httr::content(res2)), 3)
  expect_equal(length(res2$content), 35588)
})

test_that("use_cassette w/ with images: crul", {
  skip_on_cran()

  library(crul)
  url <- hb("/image/jpeg")

  ## preserve_exact_body_bytes = FALSE
  # works on 1st request - doing a real http request
  use_cassette("test_write_crul_binary_img", {
    out <- crul::HttpClient$new(url)$get()
  })

  expect_s3_class(out, "HttpResponse")
  expect_type(out$content, "raw")

  # works on 2nd request - using cassette
  use_cassette("test_write_crul_binary_img", {
    out2 <- crul::HttpClient$new(url)$get()
  })
  expect_s3_class(out2, "HttpResponse")
  expect_type(out2$content, "raw")

  expect_identical(out$content, out2$content)

  ## preserve_exact_body_bytes = TRUE
  # works on 1st request - doing a real http request
  use_cassette(
    "test_write_crul_binary_img_bytes",
    {
      res1 <- crul::HttpClient$new(url)$get()
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_s3_class(res1, "HttpResponse")
  expect_type(res1$content, "raw")

  # works on 2nd request - using cassette
  use_cassette(
    "test_write_crul_binary_img_bytes",
    {
      res2 <- crul::HttpClient$new(url)$get()
    },
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(res2, "HttpResponse")
  expect_type(res2$content, "raw")

  expect_identical(res1$content, res2$content)
})

# cleanup
files <- c(
  "test_write_httr_binary_img.yml",
  "test_write_httr_binary_img_bytes.yml",
  "test_write_crul_binary_img.yml",
  "test_write_crul_binary_img_bytes.yml"
)
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
