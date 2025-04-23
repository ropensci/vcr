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

test_that("can match by method", {
  expect_true(request_matches_one(
    "method",
    list(method = "GET"),
    list(method = "GET")
  ))
  expect_false(request_matches_one(
    "method",
    list(method = "GET"),
    list(method = "POST")
  ))
})

test_that("can match by url", {
  expect_true(request_matches_one(
    "uri",
    list(uri = "http://a.com"),
    list(uri = "http://a.com")
  ))
  expect_false(request_matches_one(
    "uri",
    list(uri = "http://a.com"),
    list(uri = "http://b.com")
  ))
})

test_that("can match by host", {
  expect_true(request_matches_one(
    "host",
    list(uri = "http://a.com/foo"),
    list(uri = "http://a.com/bar")
  ))
  expect_false(request_matches_one(
    "host",
    list(uri = "http://a.com/foo"),
    list(uri = "http://b.com/foo")
  ))
})

test_that("can match by path", {
  expect_true(request_matches_one(
    "path",
    list(uri = "http://a.com/foo"),
    list(uri = "http://b.com/foo")
  ))
  expect_true(request_matches_one(
    "path",
    list(uri = "http://a.com/"),
    list(uri = "http://b.com")
  ))
  expect_true(request_matches_one(
    "path",
    list(uri = "http://a.com/foo"),
    list(uri = "http://b.com/foo/")
  ))
  expect_false(request_matches_one(
    "path",
    list(uri = "http://a.com/foo"),
    list(uri = "http://a.com/bar")
  ))
})

test_that("can match by query", {
  expect_true(request_matches_one(
    "query",
    list(uri = "http://a.com/foo?foo=bar"),
    list(uri = "http://a.com/bar?foo=bar")
  ))
  expect_true(request_matches_one(
    "query",
    list(uri = "http://a.com/foo?foo=%C2%B5"),
    list(uri = "http://a.com/bar?foo=\u00b5")
  ))
  expect_false(request_matches_one(
    "query",
    list(uri = "http://a.com/foo?foo=bar"),
    list(uri = "http://a.com/foo?foo=baz")
  ))
})

test_that("can match by body", {
  expect_true(request_matches_one(
    "body",
    list(body = charToRaw("abc")),
    list(body = charToRaw("abc"))
  ))
  expect_false(request_matches_one(
    "body",
    list(body = charToRaw("abc")),
    list(body = charToRaw("def"))
  ))

  expect_true(request_matches_one(
    "body",
    list(body = "abc"),
    list(body = "abc")
  ))
  expect_false(request_matches_one(
    "body",
    list(body = "abc"),
    list(body = "def")
  ))
})
