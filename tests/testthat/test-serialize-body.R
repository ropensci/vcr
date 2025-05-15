test_that("to_base64 and from_base64 are idempotent", {
  string <- ""
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("a", 100)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("x", 1000)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))
})

test_that("from_base64 returns empty raw (v1 band-aid)", {
  expect_equal(from_base64(raw()), raw())
})

test_that("is_base64 works", {
  expect_false(is_base64(raw()))

  # Valid base64 strings should return TRUE
  expect_true(is_base64("aGVsbG8gd29ybGQ=")) # "hello world" in base64
  expect_true(is_base64("YWJjZGVm")) # "abcdef" in base64
  expect_true(is_base64("dGVzdA==")) # "test" in base64

  # Base64 with newlines should still work
  expect_true(is_base64("aGVsbG8g\nd29ybGQ="))
  expect_true(is_base64("aGVs\nbG8g\nd29y\nbGQ="))

  # Valid multiline base64 (like from to_base64 with long content)
  long_base64 <- paste0(strrep("ABCDEFGH", 10), "\n", strrep("IJKLMNOP", 10))
  expect_true(is_base64(long_base64))

  # Non-base64 content should return FALSE
  expect_false(is_base64("hello world")) # Plain text
  expect_false(is_base64("not base64!")) # Invalid characters
  expect_false(is_base64("aGVsbG8=world")) # Mixed content
  expect_false(is_base64("aGVsbG8===")) # Invalid padding (too many =)
  expect_false(is_base64("aGVsbG8=world=")) # Padding in wrong position

  # Edge cases
  expect_false(is_base64("")) # Empty string
  expect_false(is_base64(NA_character_)) # NA
  expect_false(is_base64(NULL)) # NULL
  expect_false(is_base64(c("YWJj", "ZGVm"))) # Vector with length > 1
  expect_false(is_base64(123)) # Non-character

  # Length not divisible by 4
  expect_false(is_base64("a"))
  expect_false(is_base64("ab"))
  expect_false(is_base64("abc"))
  expect_false(is_base64("abcde"))

  # Valid base64 that decodes to text
  text_base64 <- to_base64("This is a test string")
  expect_true(is_base64(text_base64))

  # Valid base64 that decodes to binary data
  binary_data <- as.raw(c(0x00, 0xFF, 0x10, 0xAA, 0xBB, 0xCC))
  binary_base64 <- to_base64(binary_data)
  expect_true(is_base64(binary_base64))
})

test_that("encode_body and decode_body are idempotent", {
  test_idempotent <- function(data) {
    expect_equal(decode_body(encode_body(data))$data, data)
  }

  test_idempotent(NULL)
  test_idempotent(list(a = 1, b = 2))
  test_idempotent("abcdefg")
  test_idempotent(charToRaw("abcdefg"))
})

test_that("can decode v1 bodies", {
  expect_equal(
    decode_body(list(string = "foo.txt", file = TRUE)),
    list(data = "foo.txt", on_disk = TRUE)
  )

  expect_equal(
    decode_body(list(string = FALSE, file = FALSE)),
    list(data = NULL, on_disk = FALSE)
  )

  expect_equal(
    decode_body(list(base64_string = "YWJjZGVm")),
    list(data = charToRaw("abcdef"), on_disk = FALSE)
  )
})

test_that("warns about v1 bodies with a string that's base64", {
  local_cassette("test", warn_on_empty = FALSE)

  body <- list(string = to_base64("hello world"), file = FALSE)
  expect_snapshot(out <- decode_body(body, preserve_bytes = TRUE))
  expect_equal(out$data, charToRaw("hello world"))

  # but doesn't warn if the string isn't base64
  body <- list(string = "hello world", file = FALSE)
  expect_no_warning(out <- decode_body(body, preserve_bytes = TRUE))
})

test_that("v1 bodies works for v2 vcr", {
  expect_equal(
    decode_body(list(string = raw())),
    list(data = raw(), on_disk = FALSE)
  )

  expect_equal(
    decode_body(list(string = "")),
    list(data = NULL, on_disk = FALSE)
  )
})
