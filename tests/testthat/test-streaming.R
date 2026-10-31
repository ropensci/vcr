# Reading a streamed response in chunks
test_that("downloads a large export in chunks", {
  local_vcr_configure(dir = withr::local_tempdir())
  vcr::local_cassette("export-stream")

  resp <- httr2::request("https://hb.cran.dev/stream-bytes/100000") |>
    httr2::req_perform_connection()
  withr::defer(close(resp))

  bytes <- raw()
  while (!httr2::resp_stream_is_complete(resp)) {
    bytes <- c(bytes, httr2::resp_stream_raw(resp, kb = 64))
  }

  expect_length(bytes, 100000)
})


# Reading line-delimited JSON (NDJSON)
test_that("parses each record from an NDJSON stream", {
  local_vcr_configure(dir = withr::local_tempdir())
  vcr::local_cassette("ndjson-records")

  resp <- httr2::request("https://hb.cran.dev/stream/3") |>
    httr2::req_perform_connection()
  withr::defer(close(resp))

  records <- list()
  while (!httr2::resp_stream_is_complete(resp)) {
    lines <- httr2::resp_stream_lines(resp, lines = 10)
    records <- c(records, lapply(lines, jsonlite::fromJSON))
  }

  expect_length(records, 3)
  expect_named(
    records[[1]],
    c("args", "headers", "id", "origin", "url"),
    ignore.order = TRUE
  )
})

# Server-sent events------------------------------------------------------------
test_that("collects tokens from a chat completion stream", {
  local_vcr_configure(dir = withr::local_tempdir())
  vcr::local_cassette("chat-stream", match_requests_on = c("method", "path"))

  srv <- webfakes::local_app_process(chat_app(), .local_envir = environment())

  resp <- httr2::request(srv$url("/v1/chat/completions/stream")) |>
    httr2::req_body_json(list(prompt = "hi", stream = TRUE)) |>
    httr2::req_perform_connection()
  withr::defer(close(resp))

  tokens <- character()
  while (!httr2::resp_stream_is_complete(resp)) {
    event <- httr2::resp_stream_sse(resp)
    if (!is.null(event)) tokens <- c(tokens, event$data)
  }

  expect_gt(length(tokens), 1)
})

# httr2::req_perform_stream is soft-deprecated
test_that("callback sees every chunk", {
  local_vcr_configure(dir = withr::local_tempdir())
  vcr::local_cassette("stream-callback")
  rlang::local_options(lifecycle_verbosity = "quiet")

  seen <- 0
  httr2::request("https://hb.cran.dev/stream/3") |>
    httr2::req_perform_stream(\(x) {
      seen <<- seen + length(x)
      TRUE
    })

  expect_gt(seen, 0)
})
