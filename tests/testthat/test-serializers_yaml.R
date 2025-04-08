test_that("YAML basic stuff", {
  aa <- YAML$new()
  expect_s3_class(aa, "R6")
  expect_s3_class(aa, "YAML")

  # vars
  expect_equal(aa$file_extension, ".yml")
  expect_type(aa$path, "character")
  expect_match(aa$path, "\\.yml")

  # methods
  expect_type(aa$serialize, "closure")
  expect_type(aa$deserialize, "closure")
})

test_that("YAML usage", {
  z <- YAML$new(path = "stuff")
  expect_match(z$path, "stuff.yml")
  # before file exists:
  expect_error(suppressWarnings(z$deserialize()), "cannot open")
  # after file exists, before any yaml in it:
  cat("", file = z$path)
  expect_equal(z$deserialize(), list())
  # after file exists, with yaml in it, with incomplete final line:
  cat("foo: 123\nbar: 456", file = z$path)
  expect_warning(z$deserialize(), "incomplete final line")
  # after file exists, with yaml in it, without incomplete final line:
  cat("foo: 123\nbar: 456\n", file = z$path)
  expect_type(z$deserialize(), "list")

  # cleanup
  unlink(z$path)
})

test_that("YAML fails well", {
  expect_error(YAML$new(a = 5), "unused argument")

  z <- YAML$new()
  # if no path specified, fails with useful message as is
  expect_error(suppressWarnings(z$deserialize()), "cannot open the connection")
})
test_that("Windows encoding", {
  path <- test_path("cassettes/ropenaq-encoding.yaml")
  expect_type(yaml_load_desecret(path), "list") # could fail on Windows
})
