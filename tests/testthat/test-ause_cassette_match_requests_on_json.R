test_that("use_cassette: match_requests_on - JSON-encoded body w/ crul", {
  skip_on_cran()
  skip_on_ci()

  webmockr::webmockr_reset()
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
    "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching fails when the body changes
  expect_error(
    use_cassette(
      "testing1",
      res <- cli$post("post", body = list(foo = "baz"), encode = "json"),
      match_requests_on = "body"
    ),
    "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(
    "testing1",
    res <- cli$post("post", body = list(foo = "baz"), encode = "json"),
    match_requests_on = c("uri", "method")
  )
  expect_identical(recorded_at(aa), recorded_at(cc))
})


test_that("use_cassette: match_requests_on - JSON-encoded body w/ httr", {
  skip_on_cran()
  skip_on_ci()
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
      res <- httr::POST(hb("/post"), body = list(foo = "bar1"), encode = "json"),
      match_requests_on = "body"
    ),
    "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(
    "testing2",
    res <- httr::POST(hb("/post"), body = list(foo = "bar1"), encode = "json"),
    match_requests_on = c("uri", "method")
  )
  expect_identical(recorded_at(aa), recorded_at(cc))
})
