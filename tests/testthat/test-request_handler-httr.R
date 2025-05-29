skip_on_cran()

test_that("RequestHandlerHttr: httr", {
  local_vcr_configure(dir = withr::local_tempdir())
  skip_if_not_installed("xml2")

  load("httr_obj.rda")
  x <- RequestHandlerHttr$new(httr_obj)
  expect_s3_class(x, "RequestHandlerHttr")

  # do request
  local_cassette("greencow", warn_on_empty = FALSE)
  response <- x$handle()

  expect_s3_class(response, "response")
  # status code is correct
  expect_equal(response$status_code, 404)
})

test_that("httr status code works", {
  local_vcr_configure(dir = withr::local_tempdir())
  skip_if_not_installed("xml2")

  load("httr_obj.rda")

  expect_s3_class(httr_obj, "request")

  x <- RequestHandlerHttr$new(httr_obj)
  expect_s3_class(x, "RequestHandlerHttr")

  # do request
  use_cassette("greencow", response <- x$handle())
  expect_s3_class(response, "response")
  # status code is correct
  expect_equal(response$status_code, 404)

  # call again
  use_cassette("greencow", response2 <- x$handle())
  expect_s3_class(response2, "response")
  # status code is correct
  expect_equal(response2$status_code, 404)
})

test_that('issue 249 is correctly handled.', {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette('get_401', {
    res <- httr::GET(hb('/status/401'))
  })
  expect_true(res$status_code == 401)
})

test_that("can ignore a request", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    ignore_localhost = TRUE,
    warn_on_empty_cassette = FALSE
  )

  use_cassette("test", res <- httr::GET(hb('/status/400')))
  expect_true(res$status_code == 400)
})

test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  # recorded
  use_cassette("httr_test1", x <- httr::GET(hb("/404")))
  expect_s3_class(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, hb("/404"))
  expect_s3_class(x$request, "request")
  expect_equal(x$request$method, "GET")
  expect_equal(x$request$url, hb("/404"))
  expect_named(x$request$headers, c("Content-Type", "Accept"))
  expect_null(x$request$fields)
  expect_true(x$request$options$httpget)
  expect_s3_class(x$request$output, "write_function")

  # replayed
  use_cassette("httr_test1", x2 <- httr::GET(hb("/404")))
  expect_s3_class(x2$request, "request")
  expect_equal(x2$request$method, "GET")
  expect_equal(x2$request$url, hb("/404"))
  expect_named(x2$request$headers, "Accept")
  expect_null(x2$request$fields)
  expect_true(x2$request$options$httpget)
})

test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette(
    "httr_test2",
    x <- httr::GET(hb("/404")),
    preserve_exact_body_bytes = TRUE
  )

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr_test2")

  # response
  expect_s3_class(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, hb("/404"))
})

test_that("httr w/ >1 request per cassette", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("multiple_queries_httr_record_once", {
    x404 <- httr::GET(hb("/status/404"))
    x500 <- httr::GET(hb("/status/500"))
    x418 <- httr::GET(hb("/status/418"))

    expect_equal(httr::status_code(x404), 404)
    expect_equal(httr::status_code(x500), 500)
    expect_equal(httr::status_code(x418), 418)
  })

  # response
  expect_s3_class(x404, "response")
  expect_equal(x404$status_code, 404)
  expect_s3_class(x500, "response")
  expect_equal(x500$status_code, 500)
  expect_s3_class(x418, "response")
  expect_equal(x418$status_code, 418)

  # response body
  expect_match(vcr_last_request()$uri, "418")
  expect_match(vcr_last_response()$body$string, "teapot")
})

test_that("httr works with simple auth and hides auth details", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette(
    "httr_test_simple_auth",
    x <- httr::GET(hb("/basic-auth/foo/bar"), httr::authenticate("foo", "bar"))
  )
  # successful request
  expect_equal(x$status_code, 200)

  # no auth details in the cassette
  expect_false(has_name(vcr_last_request()$headers, "Authorization"))
})

test_that("string body works", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  body <- "thisisastring"

  # Check that we make the request correctly
  use_cassette("test", res1 <- httr::POST(hb_remote("/post"), body = body))
  content1 <- httr::content(res1, "parsed")
  expect_equal(content1$data, body)

  # Check that we can replay the request and get the same response
  use_cassette("test", res2 <- httr::POST(hb("/post"), body = body))
  content2 <- httr::content(res2, "parsed")
  expect_equal(content2, content1)
})

test_that("multipart body works", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  tempfile <- withr::local_tempfile(lines = "hello world")
  body <- list(x = "1", y = httr::upload_file(tempfile))

  # Check that we make the request correctly
  use_cassette("test", res1 <- httr::POST(hb("/post"), body = body))
  content1 <- httr::content(res1, "parsed")
  expect_equal(content1$form$x, "1")
  expect_equal(content1$files$y$filename, basename(tempfile))

  # Check that we can replay the request and get the same response
  use_cassette("test", res2 <- httr::POST(hb("/post"), body = body))
  content2 <- httr::content(res2, "parsed")
  expect_equal(content2, content1)
})

test_that("empty body works", {
  local_vcr_configure(dir = withr::local_tempdir(), match_requests_on = "body")

  body <- NULL

  # Check that we make the request correctly
  use_cassette("test", res1 <- httr::POST(hb("/post"), body = body))
  content1 <- httr::content(res1, "parsed")
  expect_length(content1$data, 0)
  expect_length(content1$files, 0)
  expect_length(content1$form, 0)

  # Check that we can replay the request and get the same response
  use_cassette("test", res2 <- httr::POST(hb("/post"), body = body))
  content2 <- httr::content(res2, "parsed")
  expect_equal(content2, content1)
})

test_that("JSON-encoded body", {
  local_vcr_configure(dir = withr::local_tempdir())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar"), encode = "json"),
    match_requests_on = c("method", "uri", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar"), encode = "json"),
    match_requests_on = c("method", "uri", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  # matching fails when the body changes
  expect_error(
    use_cassette(
      "testing2",
      res <- httr::POST(hb("/post"), body = list(foo = "baz"), encode = "json"),
      match_requests_on = "body"
    ),
    class = "vcr_unhandled"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar"), encode = "json"),
    match_requests_on = c("uri", "method")
  )
  expect_identical(recorded_at(aa), recorded_at(cc))
})


test_that("binary body uses bsae64 encoding", {
  local_vcr_configure(dir = withr::local_tempdir())
  path <- file.path(withr::local_tempdir(), "test.png")

  use_cassette(
    "test",
    httr::GET(hb("/image"), httr::add_headers("Accept" = "image/png"))
  )
  expect_named(vcr_last_response()$body, "raw_gzip")
})

test_that("can write files to disk", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)

  path <- file.path(withr::local_tempdir(), "image.png")
  download_image <- function() {
    httr::GET(
      hb("/image"),
      httr::add_headers("Accept" = "image/png"),
      httr::write_disk(path, TRUE)
    )
  }
  # First request uses httr path
  use_cassette("test", out <- download_image())
  expect_equal(normalizePath(out$content), normalizePath(path))

  # First seconds uses vcr path
  use_cassette("test", out2 <- download_image())
  expect_equal(
    out2$content,
    structure(file.path(dir, "test-files", "image.png"), class = "path")
  )

  # Content is the same
  expect_equal(httr::content(out, "raw"), httr::content(out2, "raw"))
})

test_that("match_requests_on - body", {
  local_vcr_configure(dir = withr::local_tempdir())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar")),
    match_requests_on = c("method", "uri", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar")),
    match_requests_on = c("method", "uri", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  ### matchers: method, body (uri ignored essentially)
  # run it
  aa <- use_cassette(
    "testing4",
    res <- httr::POST(
      hb("/post"),
      query = list(a = 5),
      body = list(foo = "bar")
    ),
    match_requests_on = c("method", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing4",
    res <- httr::POST(
      hb("/post"),
      query = list(b = 2),
      body = list(foo = "bar")
    ),
    match_requests_on = c("method", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing4")
  expect_equal(aa$match_requests_on, c("method", "body"))

  ### matchers: body only
  # run it
  aa <- use_cassette(
    "testing5",
    res <- httr::POST(hb("/post"), body = list(foo = "bar")),
    match_requests_on = "body"
  )
  # run it again, method and uri changed
  bb <- use_cassette(
    "testing5",
    res <- httr::PUT(hb("/put"), body = list(foo = "bar")),
    match_requests_on = "body"
  )
  # run it again, method and uri changed again
  cc <- use_cassette(
    "testing5",
    res <- httr::PATCH(hb("/patch"), body = list(foo = "bar")),
    match_requests_on = "body"
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_identical(recorded_at(bb), recorded_at(cc))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing5")
  expect_equal(aa$match_requests_on, "body")
  expect_equal(bb$match_requests_on, "body")

  ### matchers: host and path only (notice how HTTP method and query are ignored)
  # run it
  aa <- use_cassette(
    "testing_httr_host_path",
    res <- httr::GET("https://ropensci.org/about", query = list(b = 99999)),
    match_requests_on = c("host", "path")
  )
  # run it again
  bb <- use_cassette(
    "testing_httr_host_path",
    res2 <- httr::POST("https://ropensci.org/about", query = list(a = 5)),
    match_requests_on = c("host", "path")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing_httr_host_path")
  expect_equal(aa$match_requests_on, c("host", "path"))
})
