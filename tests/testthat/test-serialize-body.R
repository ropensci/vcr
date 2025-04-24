test_that("to_base64 and from_base64 are idempotent", {
  string <- ""
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("a", 100)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("x", 1000)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))
})
