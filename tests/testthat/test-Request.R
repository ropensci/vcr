test_that("Request basic stuff", {
  aa <- Request$new()
  expect_s3_class(aa, "R6")
  expect_s3_class(aa, "Request")

  # vars
  expect_null(aa$disk)
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
  expect_type(aa$from_hash, "closure")
  expect_type(aa$to_hash, "closure")
})

test_that("Request usage", {
  url <- hb("/post")
  body <- list(foo = "bar")
  headers <- list(
    `User-Agent` = "libcurl/7.54.0 r-curl/3.2 crul/0.5.2",
    `Accept-Encoding` = "gzip, deflate",
    Accept = "application/json, text/xml, application/xml, */*"
  )
  aa <- Request$new("POST", url, body, headers)
  expect_equal(aa$body, "foo=bar")
  expect_equal(aa$method, "post")
  expect_equal(aa$uri, "http://127.0.0.1/post")
  expect_type(aa$host, "character")
  expect_equal(aa$path, "post")
  expect_type(aa$headers, "list")
  expect_true("User-Agent" %in% names(aa$headers))
  h <- aa$to_hash()
  expect_type(h, "list")
  zz <- aa$from_hash(h)
  expect_s3_class(zz, "Request")
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
