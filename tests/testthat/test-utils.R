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
