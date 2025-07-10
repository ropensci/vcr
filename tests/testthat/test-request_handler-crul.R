test_that("crul POST requests works", {
  withr::local_options(warnPartialMatchDollar = FALSE)
  local_vcr_configure(dir = withr::local_tempdir())

  # body type: named list
  use_cassette("crul_post_named_list", {
    x <- crul::HttpClient$new(hb("/post"))$post(body = list(foo = "bar"))
  })
  expect_s3_class(x, "HttpResponse")
  expect_equal(x$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  expect_equal(strj$form, list(foo = "bar"))

  # body type: character
  use_cassette("crul_post_string", {
    z <- crul::HttpClient$new(hb("/post"))$post(body = "some string")
  })
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: raw
  use_cassette("crul_post_raw", {
    z <- crul::HttpClient$new(hb("/post"))$post(body = charToRaw("some string"))
  })
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: upload_file
  ## upload_file in a list
  ff <- withr::local_tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  use_cassette("crul_post_upload_file", {
    b <- crul::HttpClient$new(hb("/post"))$post(
      body = list(y = crul::upload(ff))
    )
  })
  expect_s3_class(b, "HttpResponse")
  expect_equal(b$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  expect_equal(strj$files$y$filename, basename(ff)) # files not empty

  ## upload_file not in a list
  use_cassette("crul_post_upload_file_no_list", {
    d <- crul::HttpClient$new(hb("/post"))$post(
      body = crul::upload(system.file("CITATION"))
    )
  })
  expect_s3_class(d, "HttpResponse")
  expect_equal(d$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  expect_equal(length(strj$files), 0) # files empty
  expect_match(strj$data, "bibentry\\(") # data not empty

  # body type: NULL
  use_cassette("crul_post_null", {
    m <- crul::HttpClient$new(hb("/post"))$post(body = NULL)
  })
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  strj <- jsonlite::fromJSON(vcr_last_response()$body$string)
  expect_equal(strj$headers$`Content-Length`, "0")
})

test_that("JSON-encoded body", {
  local_vcr_configure(dir = withr::local_tempdir())

  cli <- crul::HttpClient$new(url = hb())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "bar"), encode = "json"),
    match_requests_on = c("method", "uri", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "bar"), encode = "json"),
    match_requests_on = c("method", "uri", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  # matching fails when comparing multipart- and json-encoded bodies
  expect_error(
    # fails when comparing multipart- and json-encoded bodies
    use_cassette(
      "testing1",
      cli$post("post", body = list(foo = "bar")),
      match_requests_on = c("method", "uri", "body")
    ),
    class = "vcr_unhandled"
  )

  # matching fails when the body changes
  expect_error(
    use_cassette(
      "testing1",
      res <- cli$post("post", body = list(foo = "baz"), encode = "json"),
      match_requests_on = "body"
    ),
    class = "vcr_unhandled"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "baz"), encode = "json"),
    match_requests_on = c("uri", "method")
  )
  expect_identical(recorded_at(aa), recorded_at(cc))
})


test_that("can write files to disk", {
  write_path <- withr::local_tempdir()
  local_vcr_configure(
    dir = withr::local_tempdir(),
    write_disk_path = write_path
  )
  path <- file.path(withr::local_tempdir(), "test.png")
  download_image <- \() {
    crul::HttpClient$new(
      url = hb("/image"),
      headers = list(Accept = "image/png")
    )$get(disk = path)
  }

  # Both requests use vcr path
  use_cassette("test3", out <- download_image())
  expect_equal(out$content, file.path(write_path, "test.png"))

  use_cassette("test3", out2 <- download_image())
  expect_equal(out2$content, file.path(write_path, "test.png"))

  # Content is the same
  expect_equal(out$parse(), out2$parse())
})

test_that("match_requests_on - body works", {
  local_vcr_configure(dir = withr::local_tempdir())
  cli <- crul::HttpClient$new(url = hb())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "bar")),
    match_requests_on = c("method", "uri", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "bar")),
    match_requests_on = c("method", "uri", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  ### matchers: method, body (uri ignored essentially)
  # run it
  aa <- use_cassette(
    "testing2",
    res <- cli$post("post", query = list(a = 5), body = list(foo = "bar")),
    match_requests_on = c("method", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing2",
    res <- cli$post("post", query = list(b = 2), body = list(foo = "bar")),
    match_requests_on = c("method", "body")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "body"))

  ### matchers: body only
  # run it
  # FIXME: still not quite working
  cli2 <- crul::HttpClient$new(url = "https://stuff.com")
  aa <- use_cassette(
    "testing3",
    res <- cli$put("put", body = list(foo = "bar")),
    match_requests_on = "body"
  )
  # run it again, method and uri changed
  bb <- use_cassette(
    "testing3",
    res2 <- cli$post("post", body = list(foo = "bar")),
    match_requests_on = "body"
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing3")
  expect_equal(aa$match_requests_on, "body")

  ### matchers: host only (note how query is ignored)
  # run it
  aa <- use_cassette(
    "testing_host1",
    res <- crul::HttpClient$new(url = hb())$get(query = list(b = 99999)),
    match_requests_on = "host"
  )
  # run it again
  bb <- use_cassette(
    "testing_host1",
    res2 <- crul::HttpClient$new(url = hb())$get(query = list(a = 5)),
    match_requests_on = "host"
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing_host1")
  expect_equal(aa$match_requests_on, "host")

  ### matchers: path only (note how host and query differences are ignored)
  # run it
  aa <- use_cassette(
    "testing_path1",
    res <- crul::HttpClient$new("https://scottchamberlain.info")$get(
      "about",
      query = list(b = 99999)
    ),
    match_requests_on = "path"
  )
  # run it again
  bb <- use_cassette(
    "testing_path1",
    res2 <- crul::HttpClient$new("https://ropensci.org")$get(
      "about",
      query = list(a = 5)
    ),
    match_requests_on = "path"
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing_path1")
  expect_equal(aa$match_requests_on, "path")

  ### matchers: host and path only (notice how HTTP method and query are ignored)
  # run it
  aa <- use_cassette(
    "testing_host_path",
    res <- crul::HttpClient$new(url = "https://ropensci.org")$get(
      "about",
      query = list(b = 99999)
    ),
    match_requests_on = c("host", "path")
  )
  # run it again
  bb <- use_cassette(
    "testing_host_path",
    res2 <- crul::HttpClient$new(url = "https://ropensci.org")$post(
      "about",
      query = list(a = 5)
    ),
    match_requests_on = c("host", "path")
  )
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing_host_path")
  expect_equal(aa$match_requests_on, c("host", "path"))
})
