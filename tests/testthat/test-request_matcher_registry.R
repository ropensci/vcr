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
  expect_equal(make_comparison("query", req), list(query = c(bar = "baz")))
})

test_that("default uri extraction ignores port", {
  req <- list(method = "GET", uri = "http://x.com:123")

  expect_equal(
    make_comparison("uri", req),
    list(uri = list(scheme = "http", host = "x.com", path = ""))
  )
  expect_equal(
    make_comparison("uri_with_port", req),
    list(uri = list(scheme = "http", host = "x.com", port = '123', path = ""))
  )
})

test_that("query params are normalized", {
  expect_equal(
    make_comparison("query", list(uri = "http://a.com/foo?foo=%C2%B5")),
    list(query = c(foo = "\u00b5"))
  )
})

test_that("query params are filtered", {
  local_vcr_configure(filter_query_parameters = "foo")

  expect_equal(
    make_comparison("query", list(uri = "http://a.com/")),
    set_names(list())
  )

  expect_equal(
    make_comparison("query", list(uri = "http://a.com/?foo=x")),
    set_names(list())
  )
})
