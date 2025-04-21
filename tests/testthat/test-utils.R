test_that("can_rawToChar", {
  # non-raw class can not be converted
  expect_false(can_rawToChar(5))
  expect_false(can_rawToChar("foo bar"))

  # raw from char can be converted
  aa <- charToRaw("foo bar")
  expect_type(aa, "raw")
  expect_true(can_rawToChar(aa))

  # raw from non-char bytes can not be converted
  # save(png_eg, file = "tests/testthat/png_eg.rda", version = 2)
  load("png_eg.rda")
  expect_type(png_eg, "raw")
  expect_false(can_rawToChar(png_eg))
})

test_that("to_base64 and from_base64 are idempotent", {
  string <- ""
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("a", 100)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("x", 1000)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))
})
