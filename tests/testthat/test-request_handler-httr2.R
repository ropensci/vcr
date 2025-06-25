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
