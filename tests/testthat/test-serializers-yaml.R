test_that("correctly computes path", {
  aa <- YAML$new("path", "name")
  expect_equal(aa$path, "path/name.yml")
})

test_that("generates expected yaml", {
  local_mocked_bindings(
    Sys.time = function() as.POSIXct("2024-01-01"),
    pkg_versions = function() "<package_versions>"
  )

  request <- Request$new("GET", uri = "http://example.com")
  response <- VcrResponse$new(200, list(name = "value"), body)
  interaction <- list(request = request, respnse = response)

  ser <- YAML$new(withr::local_tempdir(), "serialize")
  ser$serialize(list(interaction))

  expect_snapshot_file(ser$path)
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
