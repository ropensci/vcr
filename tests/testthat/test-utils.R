context("utilities")

test_that("can_rawToChar", {
  # non-raw class can not be converted
  expect_false(can_rawToChar(5))
  expect_false(can_rawToChar("foo bar"))
  
  # raw from char can be converted
  aa <- charToRaw("foo bar")
  expect_is(aa, "raw")
  expect_true(can_rawToChar(aa))

  # raw from non-char bytes can not be converted
  # save(png_eg, file = "tests/testthat/png_eg.rda", version = 2)
  load("png_eg.rda")
  expect_is(png_eg, "raw")
  expect_false(can_rawToChar(png_eg))
})

test_that("try_encoding", {
  expect_error(try_encoding(), "'x' is missing")
  expect_equal(try_encoding("foo"), "unknown")
  expect_equal(try_encoding(charToRaw("foo")), "ASCII-8BIT")
})

test_that("is_base64", {
  expect_error(is_base64(), "\"x\" is missing")
  # actual base64 strings are base64
  expect_true(is_base64(base64enc::base64encode(charToRaw("foo"))))
  # regular character strings are not base64
  expect_false(is_base64("foo"))
  # numbers as strings are not base64
  expect_false(is_base64("12345"))
  # numbers as numbers are not base64
  expect_false(is_base64(12345))
})

test_that("serializable_body", {
  expect_error(serializable_body(), "\"x\" is missing")
  expect_null(serializable_body(NULL, TRUE))
  expect_null(serializable_body(NULL, FALSE))

  aa <- serializable_body("foo", TRUE)
  expect_is(aa, "character")
  expect_true(attr(aa, "base64"))
  expect_true(is_base64(aa))
  expect_true(is_base64(aa[[1]]))

  bb <- serializable_body("foo", FALSE)
  expect_is(bb, "character")
  expect_null(attr(bb, "base64"))
  expect_false(is_base64(bb))
})

test_that("body_from", {
  expect_error(body_from(), "\"x\" is missing")

  aa <- body_from(NULL)
  expect_is(aa, "character")
  expect_equal(aa, "")

  bb <- body_from("foo")
  expect_is(bb, "character")
  expect_equal(bb, "foo")

  cc <- body_from(base64enc::base64encode(charToRaw("foo")))
  expect_is(cc, "character")
  expect_true(is_base64(base64enc::base64encode(charToRaw("foo"))))
  expect_false(is_base64(cc))

  dd <- body_from(charToRaw("foo"))
  expect_is(dd, "raw")
})

test_that("check cassette names well", {
  # no spaces
  expect_error(check_cassette_name("foo bar"), "no spaces")
  # no file ext included - just checking yml/yaml for now
  expect_error(check_cassette_name("foo.yml"), "extension")
  expect_error(check_cassette_name("foo.yaml"), "extension")
})
