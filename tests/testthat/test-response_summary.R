context("response_summary")

library("crul")
url <- "https://eu.httpbin.org"
cli <- crul::HttpClient$new(url = url)
crul::mock(FALSE)
webmockr::webmockr_allow_net_connect()

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

test_that("response_summary - with raw bytes that can't be converted to char", {
  skip_on_cran()

  load("png_eg.rda")
  status <- list(status_code = 200, message = "OK",
    explanation = "Request fulfilled, document follows")
  headers <- list(
    status = "HTTP/1.1 200 OK",
    connection = "keep-alive",
    date = "Tue, 24 Apr 2018 04:46:56 GMT"
  )
  x <- VcrResponse$new(status, headers, png_eg, "HTTP/1.1 200 OK")
  
  aa <- response_summary(x)

  expect_is(aa, "character")
  expect_match(aa, "200")
  expect_match(aa, "<raw>")
})

test_that("response_summary fails well", {
  expect_error(response_summary(), "\"response\" is missing")
  expect_error(response_summary(5), "is not TRUE")
})
