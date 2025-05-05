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
  req5 <- list(uri = "http://example.com/?x=1", method = "GET")

  expect_snapshot({
    . <- request_matches(req1, req1)
    . <- request_matches(req1, req2)
    . <- request_matches(req1, req3)
    . <- request_matches(req1, req4)
    . <- request_matches(req1, req5)
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

  expect_equal(
    make_comparison("uri", req),
    list(
      uri = list(
        scheme = "http",
        host = "x.com",
        path = "",
        params = set_names(list())
      )
    )
  )
  expect_equal(
    make_comparison("uri_with_port", req),
    list(
      uri = list(
        scheme = "http",
        host = "x.com",
        port = '123',
        path = "",
        params = set_names(list())
      )
    )
  )
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
    make_comparison("query", list(uri = "http://a.com/")),
    list(query = set_names(list()))
  )

  expect_equal(
    make_comparison("query", list(uri = "http://a.com/?foo=x")),
    list(query = set_names(list()))
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
  cas <- read_cassette("test.yml")
  expect_equal(cas$http_interactions[[1]]$request$body, set_names(list()))
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
