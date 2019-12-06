context("response_summary")

library("crul")
url <- "https://eu.httpbin.org"
cli <- crul::HttpClient$new(url = url)
crul::mock(FALSE)
webmockr::webmockr_allow_net_connect()

status <- list(status_code = 200, message = "OK",
    explanation = "Request fulfilled, document follows")

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

test_that("response_summary - handles bad multibyte characters by changing encoding", {
  skip_on_cran()

  # res <- crul::HttpClient$new("https://google.com")$get()
  # google_response <- rawToChar(res$content)
  # save(google_response, file = "tests/testthat/google_response.rda", version = 2L)
  load("google_response.rda")
  headers <- list(
    status = "HTTP/1.1 200 OK",
    `cache-control` = "private, max-age=0",
    `content-type` = "text/html; charset=ISO-8859-1"
  )
  x <- VcrResponse$new(status, headers, google_response, "HTTP/1.1 200 OK")

  # errors on print.R6
  expect_error(print(x), "multibyte")
  # errors if using the old code in response_summary w/o useBytes=TRUE
  rv <- as.numeric(sub("\\.", "", paste0(R.version$major, R.version$minor)))
  if (rv <= 353) {
    expect_is(substring(gsub("\n", " ", google_response), 1, 80), 
      "character")
  } else {
    expect_error(substring(gsub("\n", " ", google_response), 1, 80))
  }

  # response_summary doesn't error now with useBytes=TRUE
  aa <- response_summary(x)

  expect_is(aa, "character")
  expect_match(aa, "200")
  expect_match(aa, "doctype html")
})

test_that("response_summary fails well", {
  expect_error(response_summary(), "\"response\" is missing")
  expect_error(response_summary(5), "is not TRUE")
})
