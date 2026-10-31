test_that("can generate all three types of response", {
  local_vcr_configure(dir = withr::local_tempdir())

  req <- httr2::request(hb("/get"))
  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_s3_class(resp_record, "httr2_response")
  expect_equal(resp_record$status_code, 200)

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  local_vcr_configure(ignore_localhost = TRUE)
  use_cassette(
    "test",
    resp_ignore <- httr2::req_perform(req),
    warn_on_empty = FALSE
  )

  compare <- function(resp) {
    resp$request <- NULL
    resp$cache <- NULL
    resp$url <- NULL
    resp$headers["Date"] <- NULL
    resp$timing <- NULL
    resp
  }
  expect_equal(compare(resp_replay), compare(resp_record))
  expect_equal(compare(resp_ignore), compare(resp_record))
})

test_that("can capture & replay raw body", {
  local_vcr_configure(dir = withr::local_tempdir())
  request <- httr2::request(hb("/image/jpeg"))

  # Record
  use_cassette(
    "test",
    resp_record <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  resp_record <- httr2::req_perform(request)
  expect_s3_class(resp_record, "httr2_response")
  expect_length(httr2::resp_body_raw(resp_record), 4742)

  # Replay
  use_cassette(
    "test",
    resp_replay <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(resp_replay, "httr2_response")
  expect_equal(
    httr2::resp_body_raw(resp_replay),
    httr2::resp_body_raw(resp_record)
  )
})

test_that("can send an image raw body in request - w/ appropriate header", {
  local_vcr_configure(dir = withr::local_tempdir())
  request <- httr2::request(hb("/post"))
  img <- system.file(package = 'httr2', 'help/figures/logo.png')
  bin <- readBin(img, what = 'raw', n = file.size(img))
  request <- httr2::req_headers(request, 'Content-Type' = 'image/png')
  request <- httr2::req_body_raw(request, bin)

  # Record
  use_cassette(
    "test",
    resp_record <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(resp_record, "httr2_response")

  # Replay
  use_cassette(
    "test",
    resp_replay <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(resp_replay, "httr2_response")
  expect_equal(
    httr2::resp_body_raw(resp_replay),
    httr2::resp_body_raw(resp_record)
  )
})

test_that("can send an image raw body in request - w/o appropriate header", {
  local_vcr_configure(dir = withr::local_tempdir())
  request <- httr2::request(hb("/post"))
  img <- system.file(package = 'httr2', 'help/figures/logo.png')
  bin <- readBin(img, what = 'raw', n = file.size(img))
  request <- httr2::req_body_raw(request, bin)

  # Record
  use_cassette(
    "test",
    resp_record <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(resp_record, "httr2_response")

  # Replay
  use_cassette(
    "test",
    resp_replay <- httr2::req_perform(request),
    preserve_exact_body_bytes = TRUE
  )
  expect_s3_class(resp_replay, "httr2_response")
  expect_equal(
    httr2::resp_body_raw(resp_replay),
    httr2::resp_body_raw(resp_record)
  )
})

test_that("captured suppressed error", {
  local_vcr_configure(dir = withr::local_tempdir())
  request <- httr2::request(hb("/status/404"))
  request <- httr2::req_error(request, is_error = function(resp) FALSE)

  # Works when recorded
  use_cassette("test", resp_record <- httr2::req_perform(request))
  expect_s3_class(resp_record, "httr2_response")
  expect_equal(resp_record$status_code, 404)

  # And works when replayed
  use_cassette("test", resp_replay <- httr2::req_perform(request))
  expect_s3_class(resp_replay, "httr2_response")
  expect_equal(resp_replay$status_code, 404)
})

test_that("can capture errors", {
  local_vcr_configure(dir = withr::local_tempdir())

  req_404 <- httr2::request(hb("/status/404"))
  req_500 <- httr2::request(hb("/status/500"))

  # works when recorded
  use_cassette("test", {
    expect_error(httr2::req_perform(req_404), class = "httr2_http_404")
    expect_error(httr2::req_perform(req_500), class = "httr2_http_500")
  })

  # works when replayed
  use_cassette("test", {
    expect_error(httr2::req_perform(req_404), class = "httr2_http_404")
    expect_error(httr2::req_perform(req_500), class = "httr2_http_500")
  })
})

test_that("httr2 redacts auth header", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("uri", "headers")
  )

  request <- httr2::request(hb("/basic-auth/foo/bar"))
  request <- httr2::req_auth_basic(request, "foo", "bar")
  use_cassette("test", response <- httr2::req_perform(request))
  expect_equal(vcr_last_request()$headers, set_names(list()))
})

test_that("can capture body: string", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  req <- httr2::request(hb_remote("/post"))
  req <- httr2::req_body_raw(req, "body")

  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_equal(httr2::resp_body_json(resp_record)$data, "body")

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  expect_equal(
    httr2::resp_body_json(resp_record),
    httr2::resp_body_json(resp_replay)
  )
  expect_equal(vcr_last_request()$body, list(string = "body"))
})

test_that("binary body uses base64 encoding", {
  local_vcr_configure(dir = withr::local_tempdir())
  path <- file.path(withr::local_tempdir(), "test.png")

  req <- httr2::request(hb("/image"))
  req <- httr2::req_headers(req, "Accept" = "image/png")

  use_cassette("test", httr2::req_perform(req))
  expect_named(vcr_last_response()$body, "raw_gzip")
})

test_that("can capture body: json", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  req <- httr2::request(hb_remote("/post"))
  req <- httr2::req_body_json(req, list(foo = "bar"))

  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_equal(httr2::resp_body_json(resp_record)$data, "{\"foo\":\"bar\"}")

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  expect_equal(
    httr2::resp_body_json(resp_record),
    httr2::resp_body_json(resp_replay)
  )
  expect_equal(vcr_last_request()$body, list(string = "{\"foo\":\"bar\"}"))
})

test_that("can capture body: form", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  req <- httr2::request(hb("/post"))
  req <- httr2::req_body_form(req, a = "x", b = "y")

  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_equal(httr2::resp_body_json(resp_record)$form, list(a = "x", b = "y"))

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  expect_equal(
    httr2::resp_body_json(resp_record),
    httr2::resp_body_json(resp_replay)
  )
  expect_equal(vcr_last_request()$body, list(string = "a=x&b=y"))
})

test_that("can capture body: multipart", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  req <- httr2::request(hb("/post"))
  req <- httr2::req_body_multipart(req, a = "x", b = "y")

  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_equal(httr2::resp_body_json(resp_record)$form, list(a = "x", b = "y"))

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  expect_equal(
    httr2::resp_body_json(resp_record),
    httr2::resp_body_json(resp_replay)
  )
  expect_equal(vcr_last_request()$body, list(fields = list(a = "x", b = "y")))
})

test_that("can capture body: file", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  path <- withr::local_tempfile(fileext = ".txt", lines = "hello world")
  req <- httr2::request(hb_remote("/post"))
  req <- httr2::req_body_file(req, path)

  use_cassette("test", resp_record <- httr2::req_perform(req))
  expect_equal(httr2::resp_body_json(resp_record)$data, "hello world\n")

  use_cassette("test", resp_replay <- httr2::req_perform(req))
  expect_equal(
    httr2::resp_body_json(resp_record),
    httr2::resp_body_json(resp_replay)
  )
  expect_equal(vcr_last_request()$body, list(string = path))
})

test_that("redacted headers handled appropriately", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("uri", "headers")
  )

  use_cassette("redacted_httr2", {
    httr2::request(hb("/get")) %>%
      httr2::req_headers(NotASecret = "NotHidden") %>%
      httr2::req_headers_redacted(SecretHeader = "Hidden") %>%
      httr2::req_perform()
  })

  expect_equal(vcr_last_request()$headers, list(NotASecret = "NotHidden"))
})

# Streaming ---------------------------------------------------------------

stream_bytes <- function(resp, kb = 64) {
  bytes <- raw()
  while (!httr2::resp_stream_is_complete(resp)) {
    bytes <- c(bytes, httr2::resp_stream_raw(resp, kb = kb))
  }
  close(resp)
  bytes
}

is_stream <- function(resp) inherits(resp$body, "StreamingBody")

stream_sizes <- function(resp, kb = 64) {
  sizes <- integer()
  while (!httr2::resp_stream_is_complete(resp)) {
    sizes <- c(sizes, length(httr2::resp_stream_raw(resp, kb = kb)))
  }
  close(resp)
  sizes
}

record_stream_interaction <- function(
  url,
  body,
  body_chunks,
  headers = list()
) {
  current_cassette()$record_http_interaction(
    vcr_request("GET", url, headers = list()),
    vcr_response(200, headers = headers, body = body, body_chunks = body_chunks)
  )
}

test_that("can record and replay req_perform_connection()", {
  local_vcr_configure(dir = withr::local_tempdir())
  req <- httr2::request(hb("/stream-bytes/100000"))

  use_cassette("test", {
    resp_record <- httr2::req_perform_connection(req)
    body_record <- stream_bytes(resp_record)
  })
  expect_length(body_record, 100000)

  # recording didn't prevent the caller from streaming
  expect_s3_class(resp_record, "httr2_response")
  expect_true(is_stream(resp_record))

  use_cassette("test", {
    resp_replay <- httr2::req_perform_connection(req)
    body_replay <- stream_bytes(resp_replay)
  })
  expect_true(is_stream(resp_replay))
  expect_equal(resp_replay$status_code, 200)
  expect_equal(body_replay, body_record)
})

test_that("replay is a stream that reports completion correctly", {
  local_vcr_configure(dir = withr::local_tempdir())
  req <- httr2::request(hb("/stream-bytes/1000"))
  use_cassette("test", httr2::req_perform_connection(req))

  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_false(httr2::resp_stream_is_complete(resp))
  expect_length(httr2::resp_stream_raw(resp, kb = 100), 1000)
  expect_true(httr2::resp_stream_is_complete(resp))
  expect_length(httr2::resp_stream_raw(resp, kb = 100), 0)

  close(resp)
  expect_error(httr2::resp_stream_raw(resp), "already been closed")
})

test_that("chunk boundaries are saved in the cassette and replayed", {
  local_vcr_configure(dir = withr::local_tempdir())
  req <- httr2::request(hb("/drip?numbytes=4&duration=1&delay=0"))

  use_cassette("test", {
    resp <- httr2::req_perform_connection(req)
    sizes_record <- stream_sizes(resp)
  })

  response <- read_cassette("test.yml")$http_interactions[[1]]$response
  recorded <- unlist(response$body_chunks)
  expect_equal(sum(recorded), 4)
  expect_equal(sizes_record, recorded)

  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_equal(stream_sizes(resp), recorded)
})

test_that("replayed chunks match the recorded boundaries", {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/get")
  req <- httr2::request(url)

  use_cassette("test", {
    record_stream_interaction(url, "abcdefghij", body_chunks = c(3, 5, 2))
  })

  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_equal(stream_sizes(resp), c(3, 5, 2))

  # Asking for less than a chunk splits it, but never merges chunks
  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_equal(stream_sizes(resp, kb = 1 / 1024 * 2), c(2, 1, 2, 2, 1, 2))
})

test_that("cassettes without chunk information replay as a single chunk", {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/get")
  req <- httr2::request(url)

  use_cassette("test", {
    record_stream_interaction(url, "abcdefghij", body_chunks = NULL)
  })
  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_equal(stream_sizes(resp), 10)

  # Same if the saved sizes don't match the body
  use_cassette("test2", {
    record_stream_interaction(url, "abcdefghij", body_chunks = c(1, 2))
  })
  use_cassette("test2", resp <- httr2::req_perform_connection(req))
  expect_equal(stream_sizes(resp), 10)
})

test_that("a non-streaming recording can be replayed as a stream, and vice versa", {
  local_vcr_configure(dir = withr::local_tempdir())
  req <- httr2::request(hb("/stream/3"))

  use_cassette("regular", resp_regular <- httr2::req_perform(req))
  use_cassette("regular", resp <- httr2::req_perform_connection(req))
  expect_equal(
    paste(httr2::resp_stream_lines(resp, lines = 3), collapse = "\n"),
    trimws(httr2::resp_body_string(resp_regular))
  )
  close(resp)

  use_cassette("stream", httr2::req_perform_connection(req))
  use_cassette("stream", resp <- httr2::req_perform(req))
  expect_false(is_stream(resp))
  expect_equal(
    httr2::resp_body_string(resp),
    httr2::resp_body_string(resp_regular)
  )
})

test_that("can stream lines", {
  local_vcr_configure(dir = withr::local_tempdir())
  req <- httr2::request(hb("/stream/3"))

  use_cassette("test", {
    resp <- httr2::req_perform_connection(req)
    lines_record <- httr2::resp_stream_lines(resp, lines = 3)
    close(resp)
  })
  expect_length(lines_record, 3)

  use_cassette("test", {
    resp <- httr2::req_perform_connection(req)
    lines_replay <- httr2::resp_stream_lines(resp, lines = 3)
    close(resp)
  })
  expect_equal(lines_replay, lines_record)
})

test_that("can stream server sent events", {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/get")
  req <- httr2::request(url)

  body <- "data: one\n\ndata: two\n\nevent: done\ndata: three\n\n"
  use_cassette("test", {
    # split in the middle of the second event
    record_stream_interaction(
      url,
      body,
      body_chunks = c(14, 6, nchar(body) - 20)
    )
  })

  use_cassette("test", resp <- httr2::req_perform_connection(req))
  expect_equal(httr2::resp_stream_sse(resp)$data, "one")
  expect_equal(httr2::resp_stream_sse(resp)$data, "two")
  expect_equal(httr2::resp_stream_sse(resp)$type, "done")
  expect_null(httr2::resp_stream_sse(resp))
  expect_true(httr2::resp_stream_is_complete(resp))
  close(resp)
})

test_that("can record and replay req_perform_stream()", {
  local_vcr_configure(dir = withr::local_tempdir())
  local_options(lifecycle_verbosity = "quiet")
  req <- httr2::request(hb("/stream-bytes/100000"))

  perform <- function() {
    chunks <- list()
    httr2::req_perform_stream(
      req,
      \(x) {
        chunks[[length(chunks) + 1]] <<- x
        TRUE
      },
      buffer_kb = 32
    )
    unlist(chunks)
  }

  use_cassette("test", body_record <- perform())
  expect_length(body_record, 100000)
  use_cassette("test", body_replay <- perform())
  expect_equal(body_replay, body_record)
})

test_that("can capture streaming errors", {
  local_vcr_configure(dir = withr::local_tempdir())
  req_404 <- httr2::request(hb("/status/404"))

  use_cassette("test", {
    expect_error(
      httr2::req_perform_connection(req_404),
      class = "httr2_http_404"
    )
  })
  use_cassette("test", {
    expect_error(
      httr2::req_perform_connection(req_404),
      class = "httr2_http_404"
    )
  })

  # and can capture them when errors are suppressed
  req_404 <- httr2::req_error(req_404, is_error = \(resp) FALSE)
  use_cassette("test2", resp_record <- httr2::req_perform_connection(req_404))
  use_cassette("test2", resp_replay <- httr2::req_perform_connection(req_404))
  expect_equal(resp_record$status_code, 404)
  expect_equal(resp_replay$status_code, 404)
})

test_that("ignored streaming requests are still streamed", {
  local_vcr_configure(dir = withr::local_tempdir(), ignore_localhost = TRUE)
  req <- httr2::request(hb("/stream-bytes/1000"))

  use_cassette(
    "test",
    {
      resp <- httr2::req_perform_connection(req)
    },
    warn_on_empty = FALSE
  )
  expect_true(is_stream(resp))
  expect_equal(sum(stream_sizes(resp)), 1000)
})

test_that("streaming requests that aren't in the cassette error when replaying", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette(
    "test",
    httr2::req_perform_connection(httr2::request(hb("/get")))
  )
  expect_snapshot(
    use_cassette(
      "test",
      httr2::req_perform_connection(httr2::request(hb("/stream/2"))),
      record = "none"
    ),
    error = TRUE,
    transform = \(x) gsub("127.0.0.1:[0-9]+", "localhost:<port>", x)
  )
})
