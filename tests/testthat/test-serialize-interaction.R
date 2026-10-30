test_that("encode_decode_interaction roundtrip works", {
  interaction <- vcr_interaction(
    vcr_request(
      method = "GET",
      uri = "https://example.com/api",
      body = "test body",
      headers = list("Content-Type" = "application/json")
    ),
    vcr_response(
      status = 200,
      headers = list("Content-Type" = "application/json"),
      body = '{"status": "success"}',
      disk = FALSE
    ),
    recorded_at = as.POSIXct("2023-01-01 12:00:00", tz = "UTC")
  )

  # Roundtrip: encode then decode
  encoded <- encode_interaction(
    interaction,
    preserve_bytes = FALSE,
    matchers = c("method", "uri", "body", "headers")
  )
  decoded <- decode_interaction(encoded, preserve_bytes = FALSE)
  expect_equal(decoded, interaction)
})

test_that("encode_interactions includes recording metadata", {
  # Create a minimal interaction for testing
  interaction <- vcr_interaction(
    vcr_request("GET", "https://example.com"),
    vcr_response(200, body = "test")
  )
  encoded <- encode_interactions(list(interaction))

  # Check that version info is included correctly
  expect_match(encoded$recorded_with, "vcr/")
})

test_that("decode_interactions handles empty and NULL inputs", {
  # NULL case
  expect_equal(decode_interactions(NULL), list())

  # Empty list of interactions
  empty_interactions <- list(
    http_interactions = list(),
    recorded_with = "vcr/test"
  )
  decoded_empty <- decode_interactions(empty_interactions)
  expect_equal(decoded_empty$http_interactions, list())
  expect_equal(decoded_empty$recorded_with, "vcr/test")
})

test_that("decode_interaction handles legacy status format", {
  encoded <- list(
    request = list(
      method = "GET",
      uri = "https://example.com",
      headers = list()
    ),
    response = list(
      # Legacy status as list with status_code element
      status = list(status_code = "200"),
      headers = list()
    ),
    recorded_at = "2023-01-01 12:00:00"
  )

  decoded <- decode_interaction(encoded)
  expect_equal(decoded$response$status, 200)
})

test_that("preserve_bytes parameter affects body encoding", {
  interaction <- vcr_interaction(
    vcr_request("GET", "https://example.com"),
    vcr_response(200, body = "text data")
  )

  encoded_false <- encode_interaction(interaction, preserve_bytes = FALSE)
  expect_named(encoded_false$response$body, "string")

  encoded_true <- encode_interaction(interaction, preserve_bytes = TRUE)
  expect_named(encoded_true$response$body, "raw_gzip")
})

test_that("header/body only included if needed", {
  interaction <- vcr_interaction(
    vcr_request(
      method = "GET",
      uri = "http://example.com",
      body = "body",
      headers = list(x = 1, y = 2)
    ),
    vcr_response(status = 200L, list(), NULL)
  )

  encoded <- encode_interaction(interaction, matchers = c("method", "uri"))
  expect_named(encoded$request, c("method", "uri"))

  encoded <- encode_interaction(interaction, matchers = "default")
  expect_named(encoded$request, c("method", "uri", "body"))

  encoded <- encode_interaction(interaction, matchers = "headers")
  expect_named(encoded$request, c("method", "uri", "headers"))
})

test_that("body_chunks survive an encode/decode roundtrip", {
  interaction <- vcr_interaction(
    vcr_request("GET", "https://example.com/stream"),
    vcr_response(200, body = "abcdefghij", body_chunks = c(3, 5, 2)),
    recorded_at = as.POSIXct("2023-01-01 12:00:00", tz = "UTC")
  )

  encoded <- encode_interaction(interaction)
  expect_equal(encoded$response$body_chunks, list(3L, 5L, 2L))
  expect_equal(decode_interaction(encoded), interaction)

  # and through the file formats, where length one vectors are unboxed
  for (type in c("yaml", "json")) {
    dir <- withr::local_tempdir()
    serializer <- serializer_fetch(type, dir, "test")
    for (chunks in list(c(3, 5, 2), 10)) {
      interaction$response$body_chunks <- as.integer(chunks)
      serializer$serialize(list(interaction))
      expect_equal(serializer$deserialize()$http_interactions[[1]], interaction)
    }
  }
})

test_that("responses without body_chunks don't write the field", {
  response <- vcr_response(200, body = "abc")
  expect_null(response$body_chunks)
  expect_false("body_chunks" %in% names(encode_response(response)))
  expect_null(decode_body_chunks(NULL))
})

test_that("vcr_response() validates body_chunks", {
  expect_error(vcr_response(200, body_chunks = "a"), "body_chunks")
  expect_error(vcr_response(200, body_chunks = -1), "body_chunks")
  expect_error(vcr_response(200, body_chunks = NA_real_), "body_chunks")
})
