test_that("serializer_fetch", {
  z <- Serializer$new("path", "name", ".ext")
  expect_s3_class(z, "Serializer")
  expect_equal(z$path, "path/name.ext")
  expect_equal(z$file_extension, ".ext")
})

test_that("serializer_fetch", {
  z <- serializer_fetch("yaml", "path", "name")
  expect_s3_class(z, "YAML")
})

test_that("useful error if not registered", {
  expect_snapshot(serializer_fetch("foo"), error = TRUE)
})

test_that("you can record a new cassette of same name with different serializer", {
  local_vcr_configure(dir = withr::local_tempdir())

  cas_yml <- use_cassette(
    name = "testing1",
    res <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "yaml"
  )
  expect_match(cas_yml$file(), ".yml")
  cas_json <- use_cassette(
    name = "testing1",
    res2 <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "json"
  )
  expect_match(cas_json$file(), ".json")

  expect_equal(cas_yml$name, cas_json$name)
})

# Binary data ------------------------------------------------------------------

test_that("warns if you reload string with preserve_exact_body_bytes", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("test", httr::GET(hb("/get")))

  expect_warning(use_cassette(
    "test",
    httr::GET(hb("/get")),
    preserve_exact_body_bytes = TRUE
  ))
})

test_that("use_cassette w/ with images: httr", {
  skip_on_cran()
  skip_if_not_installed("jpeg")
  local_vcr_configure(dir = withr::local_tempdir())

  url <- hb("/image/jpeg")

  ## preserve_exact_body_bytes = FALSE
  # works on 1st request - doing a real http request
  use_cassette(
    "test_write_httr_binary_img",
    out <- httr::GET(url)
  )

  expect_s3_class(out, "response")
  expect_type(out$content, "raw")
  expect_equal(dim(httr::content(out)), c(76, 100, 3))

  # works on 2nd request - using cassette
  use_cassette(
    "test_write_httr_binary_img",
    out2 <- httr::GET(url)
  )
  expect_s3_class(out2, "response")
  expect_type(out2$content, "raw")
  expect_equal(dim(httr::content(out2)), c(76, 100, 3))

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
  expect_equal(dim(httr::content(res1)), c(76, 100, 3))
  expect_equal(length(res1$content), 4742)

  use_cassette(
    "test_write_httr_binary_img_bytes",
    {
      res2 <- httr::GET(url)
    },
    preserve_exact_body_bytes = TRUE
  )

  expect_s3_class(res2, "response")
  expect_type(res2$content, "raw")
  expect_equal(dim(httr::content(res2)), c(76, 100, 3))
  expect_equal(length(res2$content), 4742)
})

test_that("use_cassette w/ with images: crul", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

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
