context("FileSystem")

test_that("FileSystem", {
  expect_is(FileSystem, "R6ClassGenerator")
  aa <- FileSystem$new()
  expect_is(aa,  "R6")
  expect_is(aa,  "FileSystem")
  expect_null(aa$content)
  expect_null(aa$file_name)
  expect_is(aa$get_cassette,  "function")
  expect_false(aa$write2disk)
  expect_null(aa$write_fxn)
})

test_that("FileSystem fails well", {
  expect_error(FileSystem$new(a = 5), "unused argument")

  z <- FileSystem$new()
  expect_error(z$get_cassette(), "No file name provided")
  expect_error(z$is_empty(), "No file name provided")
  expect_error(z$set_cassette(), "No file name provided")
})
