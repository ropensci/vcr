context("Request")

test_that("Request basic stuff", {
  expect_is(Request, "R6ClassGenerator")
  aa <- Request$new()
  expect_is(aa,  "R6")
  expect_is(aa,  "Request")

  # vars
  expect_null(aa$disk)
  expect_null(aa$hash)
  expect_null(aa$headers)
  expect_null(aa$host)
  expect_null(aa$method)
  expect_null(aa$opts)
  expect_null(aa$path)
  expect_null(aa$query)
  expect_null(aa$scheme)
  expect_null(aa$uri)
  expect_false(aa$skip_port_stripping)

  # methods
  expect_is(aa$from_hash,  "function")
  expect_is(aa$to_hash,  "function")
})

test_that("Request usage", {
  url <- "https://eu.httpbin.org/post"
  body <- list(foo = "bar")
  headers <- list(
    `User-Agent` = "libcurl/7.54.0 r-curl/3.2 crul/0.5.2",
    `Accept-Encoding` = "gzip, deflate",
    Accept = "application/json, text/xml, application/xml, */*"
  )
  aa <- Request$new("POST", url, body, headers)
  expect_is(aa$body, "character")
  expect_equal(aa$body, "foo=bar")
  expect_is(aa$method, "character")
  expect_equal(aa$method, "post")
  expect_is(aa$uri, "character")
  expect_equal(aa$uri, "https://eu.httpbin.org/post")
  expect_is(aa$host, "character")
  expect_equal(aa$host, "eu.httpbin.org")
  expect_is(aa$path, "character")
  expect_equal(aa$path, "post")
  expect_is(aa$headers, "list")
  expect_true("User-Agent" %in% names(aa$headers))
  h <- aa$to_hash()
  expect_is(h, "list")
  zz <- aa$from_hash(h)
  expect_is(zz, "Request")
  # hash in aa is a list
  aa$hash <- NULL
  # equal but not identical
  expect_equal(zz, aa)
})

test_that("Request fails well", {
  expect_error(Request$new(a = 5), "unused argument")

  z <- Request$new()
  expect_error(z$foo(), "attempt to apply non-function")
  expect_error(z$from_hash(), "missing")
  expect_error(z$to_hash(4), "unused argument")
})
