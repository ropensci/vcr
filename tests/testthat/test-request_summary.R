context("request_summary")

library("crul")
url <- "https://eu.httpbin.org"
cli <- crul::HttpClient$new(url = url)
crul::mock(FALSE)
webmockr::webmockr_allow_net_connect()

test_that("request_summary works", {
  skip_on_cran()

  body <- list(foo = "bar")
  res <- cli$post("post", body = body)

  x <- Request$new("POST", url, body, res$request_headers)

  aa <- request_summary(request = x, c('method', 'uri'))
  bb <- request_summary(request = x, c('method', 'uri', 'body'))
  cc <- request_summary(request = x, c('method', 'uri', 'headers'))
  dd <- request_summary(request = x, c('method', 'uri', 'body', 'headers'))

  expect_is(aa, "character")
  expect_is(bb, "character")
  expect_is(cc, "character")
  expect_is(dd, "character")

  expect_match(aa, "post https://eu.httpbin.org/")
  expect_match(bb, "post https://eu.httpbin.org/ foo=bar")

  expect_match(cc, "post")
  expect_match(cc, "https://eu.httpbin.org/")
  expect_match(cc, "libcurl")
  expect_match(cc, "r-curl")
  expect_match(cc, "crul")
  expect_match(cc, "gzip")
  expect_match(cc, "application/json")
  expect_false(grepl("foo=bar", cc))

  expect_match(dd, "post")
  expect_match(dd, "https://eu.httpbin.org/")
  expect_match(dd, "foo=bar")
  expect_match(dd, "libcurl")
  expect_match(dd, "r-curl")
  expect_match(dd, "crul")
  expect_match(dd, "application/json")
})

test_that("request_summary fails well", {
  expect_error(request_summary(), "\"request\" is missing")
  expect_error(request_summary(5), "is not TRUE")
})





context("response_summary")

test_that("response_summary works", {
  skip_on_cran()

  res <- cli$get("get", query = list(q = "stuff"))
  x <- VcrResponse$new(res$status_http(), res$response_headers,
     res$parse("UTF-8"), res$response_headers$status)

  aa <- response_summary(x)

  expect_is(aa, "character")
  expect_match(aa, "200")
  expect_match(aa, "args")
  expect_match(aa, "headers")
  expect_match(aa, "Accept")
})

test_that("response_summary fails well", {
  expect_error(response_summary(), "\"response\" is missing")
  expect_error(response_summary(5), "is not TRUE")
})
