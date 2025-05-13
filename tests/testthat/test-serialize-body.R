test_that("to_base64 and from_base64 are idempotent", {
  string <- ""
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("a", 100)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))

  string <- strrep("x", 1000)
  expect_equal(from_base64(to_base64(string)), charToRaw(string))
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
})
