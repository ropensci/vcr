test_that("has_text_content handles headers correctly", {
  # When content-type is not present
  expect_false(has_text_content(list()))
  expect_false(has_text_content(list(other = "value")))

  # Case insensitive matching for content-type header
  expect_false(has_text_content(list(`content-type` = "image/jpeg")))
  expect_false(has_text_content(list(`Content-Type` = "image/jpeg")))

  # Known text type
  expect_true(has_text_content(list(`Content-Type` = "application/json")))
})

test_that("is_binary_type correctly identifies binary content types", {
  # Text types are not binary
  expect_true(is_text_type("text/plain"))
  expect_true(is_text_type("text/html"))
  expect_true(is_text_type("text/csv"))

  # Special case application types that are not binary
  expect_true(is_text_type("application/json"))
  expect_true(is_text_type("application/xml"))
  expect_true(is_text_type("application/x-www-form-urlencoded"))
  expect_true(is_text_type("multipart/form-data"))

  # Binary content types
  expect_false(is_text_type("image/jpeg"))
  expect_false(is_text_type("image/png"))
  expect_false(is_text_type("application/pdf"))
  expect_false(is_text_type("application/octet-stream"))
  expect_false(is_text_type("audio/mpeg"))
  expect_false(is_text_type("video/mp4"))
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
