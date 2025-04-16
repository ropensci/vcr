test_that("correctly computes path", {
  aa <- YAML$new("path", "name")
  expect_equal(aa$path, "path/name.yml")
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

test_that("YAML fails well", {
  expect_error(YAML$new(a = 5), "unused argument")

  z <- YAML$new("path", "name")
  # if no path specified, fails with useful message as is
  expect_error(suppressWarnings(z$deserialize()), "cannot open the connection")
})
test_that("Windows encoding", {
  path <- test_path("cassettes/ropenaq-encoding.yaml")
  expect_type(yaml_load_desecret(path), "list") # could fail on Windows
})
