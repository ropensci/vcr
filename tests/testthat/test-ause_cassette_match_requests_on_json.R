teardown({
  vcr_configure_reset()
})

test_that("use_cassette: match_requests_on - JSON-encoded body w/ crul", {
  skip_on_cran()
  skip_on_ci()

  webmockr::webmockr_reset()
  
  on.exit(unlink(mydir, recursive = TRUE))
  mydir <- file.path(tempdir(), "crul_json_encoding")
  invisible(vcr_configure(dir = mydir))
  
  library(crul)
  cli <- HttpClient$new(url = "https://httpbin.org")

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(name = "testing1", {
    res <- cli$post("post", body = list(foo = "bar"), encode = "json")
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing1", {
    res <- cli$post("post", body = list(foo = "bar"), encode = "json")
  }, match_requests_on = c("method", "uri", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  # matching fails when comparing multipart- and json-encoded bodies
  expect_error(
   # fails when comparing multipart- and json-encoded bodies
   use_cassette(name = "testing1", {
    cli$post("post", body = list(foo = "bar"))
    }, 
    match_requests_on = c("method", "uri", "body")),
   "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching fails when the body changes
  expect_error(
    use_cassette(name = "testing1", {
      res <- cli$post("post", body = list(foo = "baz"), encode = "json")
    }, match_requests_on = "body"),
    "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(name = "testing1", {
      res <- cli$post("post", body = list(foo = "baz"), encode = "json")
    }, match_requests_on = c("uri", "method"))
  expect_identical(recorded_at(aa), recorded_at(cc))
})


test_that("use_cassette: match_requests_on - JSON-encoded body w/ httr", {
  skip_on_cran()
  skip_on_ci()

  library(httr)
  on.exit(unlink(mydir, recursive = TRUE))
  mydir <- file.path(tempdir(), "httr_json_encoding")
  invisible(vcr_configure(dir = mydir))

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(name = "testing2", {
    res <- POST("https://httpbin.org/post", body = list(foo = "bar"), encode = "json")
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing2", {
    res <- POST("https://httpbin.org/post", body = list(foo = "bar"), encode = "json")
  }, match_requests_on = c("method", "uri", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  # matching fails when the body changes
  expect_error(
    use_cassette(name = "testing2", {
      res <- POST("https://httpbin.org/post", body = list(foo = "bar1"), encode = "json")
    }, match_requests_on = "body"),
    "An HTTP request has been made that vcr does not know how to handle"
  )

  # matching succeeds when the changed body is ignored
  cc <- use_cassette(name = "testing2", {
      res <- POST("https://httpbin.org/post", body = list(foo = "bar1"), encode = "json")
    }, match_requests_on = c("uri", "method"))
  expect_identical(recorded_at(aa), recorded_at(cc))
})

