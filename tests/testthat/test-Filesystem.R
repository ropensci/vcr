test_that("FileSystem", {
  aa <- FileSystem$new()
  expect_s3_class(aa, "R6")
  expect_s3_class(aa, "FileSystem")
  expect_null(aa$content)
  expect_null(aa$file_name)
  expect_type(aa$get_cassette, "closure")
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
