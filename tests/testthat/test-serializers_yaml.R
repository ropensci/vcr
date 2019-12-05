context("serializers: YAML")

test_that("YAML basic stuff", {
  expect_is(YAML, "R6ClassGenerator")
  aa <- YAML$new()
  expect_is(aa, "R6")
  expect_is(aa, "YAML")

  # vars
  expect_is(aa$file_extension, "character")
  expect_equal(aa$file_extension, ".yml")
  expect_is(aa$path, "character")
  expect_match(aa$path, "\\.yml")
  expect_null(aa$string)

  # methods
  expect_is(aa$serialize, "function")
  expect_is(aa$deserialize_string, "function")
  expect_is(aa$deserialize_path, "function")
})

test_that("YAML usage", {
  z <- YAML$new()
  expect_true(is.null(z$string))
  expect_equal(z$deserialize_string("foo: 123\nbar: 456"),
    list(foo = 123L, bar = 456L))

  z <- YAML$new(string = "foo: 123\nbar: 456")
  expect_false(is.null(z$string))
  expect_equal(z$deserialize_string(),
    list(foo = 123L, bar = 456L))

  z <- YAML$new(path = "stuff")
  expect_match(z$path, "stuff.yml")
  # before file exists:
  expect_error(suppressWarnings(z$deserialize_path()), "cannot open")
  # after file exists, before any yaml in it:
  cat("", file = z$path)
  expect_equal(z$deserialize_path(), list())
  # after file exists, with yaml in it, with incomplete final line:
  cat("foo: 123\nbar: 456", file = z$path)
  expect_warning(z$deserialize_path(), "incomplete final line")
  # after file exists, with yaml in it, without incomplete final line:
  cat("foo: 123\nbar: 456\n", file = z$path)
  expect_is(z$deserialize_path(), "list")

  # cleanup
  unlink(z$path)
})

test_that("YAML fails well", {
  expect_error(YAML$new(a = 5), "unused argument")

  z <- YAML$new()
  # if no string specified, fails with message
  expect_error(z$deserialize_string(), "Must pass a string")
  # if no path specified, fails with useful message as is
  expect_error(suppressWarnings(z$deserialize_path()),
    "cannot open the connection")
})
