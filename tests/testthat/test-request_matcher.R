test_that("multiple matchers requires all to match", {
  expect_true(request_matches(
    list(method = "GET", uri = "http://a.com"),
    list(method = "GET", uri = "http://a.com"),
    c("method", "uri")
  ))
  expect_false(request_matches(
    list(method = "GET", uri = "http://a.com"),
    list(method = "GET", uri = "http://b.com"),
    c("method", "uri")
  ))
})

test_that("request_matches has useful logging", {
  local_vcr_configure_log(file = stdout())

  req1 <- list(uri = "http://example.com", method = "GET")
  req2 <- list(uri = "http://example.com/foo", method = "GET")
  req3 <- list(uri = "http://example.com", method = "POST")
  req4 <- list(uri = "http://example.com/foo", method = "POST")

  expect_snapshot({
    . <- request_matches(req1, req1)
    . <- request_matches(req1, req2)
    . <- request_matches(req1, req3)
    . <- request_matches(req1, req4)
  })
})

test_that("informative feedback for components that are absent", {
  local_vcr_configure_log(file = stdout())

  req1 <- list(uri = "http://example.com", method = "GET")
  req2 <- list(uri = "http://example.com?q=1", method = "GET")

  expect_snapshot({
    . <- request_matches(req1, req2)
  })
})

test_that("query parameters are normalised", {
  expect_true(request_matches(
    list(uri = "http://a.com/foo?foo=%C2%B5"),
    list(uri = "http://a.com/bar?foo=\u00b5"),
    "query"
  ))
})

test_that("make_comparison extracts expected componets", {
  req <- list(
    method = "GET",
    uri = "http://a.com/foo?bar=baz",
    body = "body",
    headers = list(name = "value")
  )

  # Straightforward extraction
  expect_equal(make_comparison("body", req), req["body"])
  expect_equal(make_comparison("headers", req), req["headers"])
  expect_equal(make_comparison("method", req), req["method"])

  # URI manipulation
  expect_equal(make_comparison("host", req), list(host = "a.com"))
  expect_equal(make_comparison("path", req), list(path = "/foo"))
  expect_equal(make_comparison("query", req), list(query = list(bar = "baz")))
})

test_that("default uri extraction ignores port", {
  req <- list(method = "GET", uri = "http://x.com:123")

  expect_equal(make_comparison("uri", req)$uri$port, NULL)
  expect_equal(make_comparison("uri_with_port", req)$uri$port, "123")
})

test_that("query params are normalized", {
  expect_equal(
    make_comparison("query", list(uri = "http://a.com/foo?foo=%C2%B5")),
    list(query = list(foo = "\u00b5"))
  )
})

test_that("query params are filtered", {
  local_vcr_configure(filter_query_parameters = "foo")

  expect_equal(
    make_comparison("query", list(uri = "http://a.com/"))$query,
    set_names(list())
  )

  expect_equal(
    make_comparison("query", list(uri = "http://a.com/?foo=x"))$query,
    set_names(list())
  )
})

test_that("json_body ignores representation", {
  req1 <- list(body = '{"foo": "bar"}')
  req2 <- list(body = '{"foo":     "bar"}')

  expect_equal(
    make_comparison("body_json", req1),
    list(body = list(foo = "bar"))
  )

  expect_true(request_matches(req1, req2, "body_json"))
})

# End to end tests -------------------------------------------------------------

test_that("can match empty bodies", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("method", "uri", "body")
  )
  cli <- crul::HttpClient$new(url = hb())

  use_cassette("test", res1 <- cli$post("post"))
  expect_null(res1$request$body)
  use_cassette("test", res1_replay <- cli$post("post"))
  expect_null(res1_replay$request$body)

  # the request body in the cassette is empty
  expect_false(has_name(vcr_last_request(), "body"))
})

test_that("can match json bodies", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("method", "uri", "body_json")
  )

  req <- httr2::request(hb("/post"))
  req <- httr2::req_body_json(req, list(foo = "bar"))
  # record
  use_cassette("test", res1 <- httr2::req_perform(req))
  # replay
  use_cassette("test", res1 <- httr2::req_perform(req))

  expect_equal(vcr_last_request()$body, list(string = '{"foo":"bar"}'))
})


test_that("default matcher includes body_json", {
  local_vcr_configure(dir = withr::local_tempdir())
  local_vcr_configure_log(file = stdout())

  req <- httr2::request(hb("/post"))
  req1 <- httr2::req_body_json(req, list(foo = "bar"))
  req2 <- httr2::req_body_json(req, list(foo = "baz"))

  use_cassette("test", httr2::req_perform(req1))
  expect_snapshot(
    use_cassette("test", httr2::req_perform(req2)),
    error = TRUE,
    transform = \(x) gsub(hb(), "{httpbin}", x, fixed = TRUE),
  )
})

test_that("default matcher includes body", {
  local_vcr_configure(dir = withr::local_tempdir())
  local_vcr_configure_log(file = stdout())

  req <- httr2::request(hb("/post"))
  req1 <- httr2::req_body_form(req, foo = "bar")
  req2 <- httr2::req_body_form(req, foo = "baz")

  use_cassette("test", httr2::req_perform(req1))
  expect_snapshot(
    use_cassette("test", httr2::req_perform(req2)),
    error = TRUE,
    transform = \(x) gsub(hb(), "{httpbin}", x, fixed = TRUE),
  )
})


test_that('request matching is not sensitive to escaping special characters', {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/get?update=2022-01-01T00:00:00&p2=ok")

  # curl does not escape
  aa <- use_cassette('test', res <- crul::HttpClient$new(url)$get())
  expect_true(res$status_code == 200)

  # httr does escape
  bb <- use_cassette('test', res <- httr::GET(url))
  expect_true(res$status_code == 200)
})
