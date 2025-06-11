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

test_that("use_cassette w/ with images: httr", {
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

# JSON -------------------------------------------------------------------------

test_that("generates correct path", {
  aa <- JSON$new("path", "name")
  expect_equal(aa$path, "path/name.json")
})

test_that("generates expected json", {
  local_vcr_configure(json_pretty = TRUE)
  local_mocked_bindings(
    Sys.time = function(tz) as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    pkg_versions = function() "<package_versions>"
  )

  interaction <- vcr_interaction(
    vcr_request(method = "GET", uri = "http://example.com"),
    vcr_response(status = 200L, list(name = "val"), body = "body")
  )
  ser <- JSON$new(withr::local_tempdir(), "serialize")
  ser$serialize(list(interaction))

  expect_snapshot(writeLines(readLines(ser$path)))
})

test_that("JSON usage", {
  local_vcr_configure(dir = withr::local_tempdir(), serialize_with = "json")

  # does one request work?
  aa <- use_cassette(
    "testing2",
    res <- crul::HttpClient$new(hb("/get"))$get()
  )
  expect_s3_class(aa, "Cassette")
  expect_s3_class(res, "HttpResponse")
  expect_length(jsonlite::fromJSON(aa$file(), FALSE)[[1]], 1)

  # do two requests work?
  cc <- use_cassette("testing4", {
    ref <- crul::HttpClient$new(hb("/get"))$get()
    the <- crul::HttpClient$new(hb("/post"))$post(body = "fafaa")
  })
  expect_s3_class(ref, "HttpResponse")
  expect_s3_class(the, "HttpResponse")
  expect_length(jsonlite::fromJSON(cc$file(), FALSE)[[1]], 2)

  # preserve exact body bytes
  dd <- use_cassette(
    "testing5",
    {
      raf <- crul::HttpClient$new(hb("/get"))$get(
        query = list(cheese = "string")
      )
      raz <- crul::HttpClient$new(hb("/post"))$post(
        body = list(foo = "bar", baz = "ball")
      )
    },
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(raf, "HttpResponse")
  expect_s3_class(raz, "HttpResponse")
  expect_length(jsonlite::read_json(dd$file())[[1]], 2)
  expect_named(jsonlite::fromJSON(dd$file())[[1]]$response$body, "raw_gzip")
})

# YAML -------------------------------------------------------------------------

test_that("correctly computes path", {
  aa <- YAML$new("path", "name")
  expect_equal(aa$path, "path/name.yml")
})

test_that("generates expected yaml", {
  local_mocked_bindings(
    Sys.time = function(tz) as.POSIXct("2024-01-01 12:00:00", tz = "UTC"),
    pkg_versions = function() "<package_versions>"
  )

  interaction <- vcr_interaction(
    vcr_request(method = "GET", uri = "http://example.com"),
    vcr_response(status = 200L, list(name = "val"), body = "body")
  )
  ser <- YAML$new(withr::local_tempdir(), "serialize")
  ser$serialize(list(interaction))

  expect_snapshot(writeLines(readLines(ser$path)))
})

test_that("YAML usage", {
  z <- YAML$new(withr::local_tempdir(), "name")
  expect_equal(basename(z$path), "name.yml")

  # before file exists:
  expect_error(suppressWarnings(z$deserialize()), "cannot open")

  # after file exists, before any yaml in it:
  file.create(z$path)
  expect_equal(z$deserialize(), list())
  # after file exists, with yaml in it, with incomplete final line:
  cat("foo: 123\nbar: 456", file = z$path)
  expect_warning(z$deserialize(), "incomplete final line")
  # after file exists, with yaml in it, without incomplete final line:
  cat("foo: 123\nbar: 456\n", file = z$path)
  expect_type(z$deserialize(), "list")
})

test_that("Windows encoding", {
  ser <- YAML$new(test_path("cassettes"), "ropenaq-encoding")

  expect_type(ser$deserialize(), "list") # could fail on Windows
})

# QS2 -------------------------------------------------------------------

test_that("generates correct path", {
  aa <- QS2$new("path", "name")
  expect_equal(aa$path, "path/name.qs2")
})

test_that("qs2 is idempotent", {
  local_mocked_bindings(
    Sys.time = function(tz) as.POSIXct("2024-01-01 12:00:00", tz = "UTC")
  )

  interaction <- vcr_interaction(
    vcr_request(method = "GET", uri = "http://example.com/"),
    vcr_response(status = 200L, list(name = "val"), body = "body")
  )

  ser <- QS2$new(withr::local_tempdir(), "name")
  ser$serialize(list(interaction))
  expect_equal(list(interaction), ser$deserialize()$http_interactions)
})
