test_that("has_binary_content handles headers correctly", {
  # When content-type is not present
  expect_false(has_binary_content(list()))
  expect_false(has_binary_content(list(other = "value")))

  # Case insensitive matching for content-type header
  expect_true(has_binary_content(list(`content-type` = "image/jpeg")))
  expect_true(has_binary_content(list(`Content-Type` = "image/jpeg")))
})

test_that("is_binary_type correctly identifies binary content types", {
  # Text types are not binary
  expect_false(is_binary_type("text/plain"))
  expect_false(is_binary_type("text/html"))
  expect_false(is_binary_type("text/csv"))

  # Special case application types that are not binary
  expect_false(is_binary_type("application/json"))
  expect_false(is_binary_type("application/xml"))
  expect_false(is_binary_type("application/x-www-form-urlencoded"))
  expect_false(is_binary_type("multipart/form-data"))

  # Binary content types
  expect_true(is_binary_type("image/jpeg"))
  expect_true(is_binary_type("image/png"))
  expect_true(is_binary_type("application/pdf"))
  expect_true(is_binary_type("application/octet-stream"))
  expect_true(is_binary_type("audio/mpeg"))
  expect_true(is_binary_type("video/mp4"))
})

test_that("parse_content_type correctly parses content type headers", {
  # Standard content types
  parsed <- parse_content_type("text/plain")
  expect_equal(parsed$type, "text")
  expect_equal(parsed$subtype, "plain")
  expect_equal(parsed$suffix, "")

  # Content type with suffix
  parsed <- parse_content_type("application/vnd.api+json")
  expect_equal(parsed$type, "application")
  expect_equal(parsed$subtype, "vnd.api")
  expect_equal(parsed$suffix, "json")

  # Content type with parameters
  parsed <- parse_content_type("text/plain; charset=utf-8")
  expect_equal(parsed$type, "text")
  expect_equal(parsed$subtype, "plain")
  expect_equal(parsed$suffix, "")

  # Invalid content type
  parsed <- parse_content_type("invalid-content-type")
  expect_equal(parsed$type, "")
  expect_equal(parsed$subtype, "")
  expect_equal(parsed$suffix, "")
})
