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

test_that("is_base64", {
  not64 <- Cassette$new('notbase64')
  is64 <- Cassette$new('isbase64', preserve_exact_body_bytes = TRUE)

  expect_error(is_base64(), "\"x\" is missing")
  expect_false(is_base64(jsonlite::base64_enc(charToRaw("foo")), not64))

  expect_true(is_base64(list(base64_string = "adfadsf"), is64))
  expect_message(z <- is_base64(list(string = "adfadsf"), is64))
  expect_true(z)

  expect_false(is_base64(list(string = "adfadsf"), not64))
  expect_true(is_base64(list(base64_string = "adfadsf"), not64))
  # # actual base64 strings are base64
  # expect_true(is_base64(jsonlite::base64_enc(charToRaw("foo"))))
  # # regular character strings are not base64
  # expect_false(is_base64("foo"))
  # # numbers as strings are not base64
  # expect_false(is_base64("12345"))
  # # numbers as numbers are not base64
  # expect_false(is_base64(12345))
})

test_that("serializable_body", {
  expect_null(serializable_body(NULL, TRUE))
  expect_null(serializable_body(NULL, FALSE))

  aa <- serializable_body("foo", TRUE)
  expect_type(aa, "character")
  expect_true(attr(aa, "base64"))
  expect_true(is_base64(aa))
  expect_false(is_base64(aa[[1]]))

  bb <- serializable_body("foo", FALSE)
  expect_type(bb, "character")
  expect_null(attr(bb, "base64"))
  expect_false(is_base64(bb))
})

test_that("body_from", {
  expect_error(body_from(), "\"x\" is missing")

  aa <- body_from(NULL)
  expect_equal(aa, "")

  bb <- body_from("foo")
  expect_equal(bb, "foo")

  cc <- body_from(jsonlite::base64_enc(charToRaw("foo")))
  expect_type(cc, "character")
  expect_false(is_base64(jsonlite::base64_enc(charToRaw("foo"))))
  expect_false(is_base64(cc))

  dd <- body_from(charToRaw("foo"))
  expect_type(dd, "raw")
})
