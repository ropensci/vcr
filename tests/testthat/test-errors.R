test_that("UnhandledHTTPRequestError fails well", {
  local_cassette("test", warn_on_empty = FALSE)
  expect_snapshot(UnhandledHTTPRequestError$new(5), error = TRUE)
})

test_that("informative error if no cassette active", {
  request <- vcr_request("post", "http://example.com")
  err <- UnhandledHTTPRequestError$new(request)
  expect_snapshot(err$construct_message(), error = TRUE)
})

test_that("UnhandledHTTPRequestError works as expected", {
  local_cassette("turtle", warn_on_empty = FALSE)

  request <- vcr_request("post", "http://example.com")
  err <- UnhandledHTTPRequestError$new(request)
  expect_snapshot(err$construct_message(), error = TRUE)
})

test_that("sensitive data is redacted in url", {
  local_cassette("bunny", warn_on_empty = FALSE)

  foo <- "abc"
  local_vcr_configure(filter_sensitive_data = list("<<foo>" = foo))
  url <- paste0("http://example.com/?foo=", foo, "&bar=", bar)
  request <- vcr_request("get", url)

  err <- UnhandledHTTPRequestError$new(request)
  cnd <- catch_cnd(err$construct_message(), classes = "error")
  expect_no_match(conditionMessage(cnd), foo)
})

test_that("sensitive data is redacted in header", {
  local_cassette(
    "test",
    match_requests_on = c("method", "uri", "headers"),
    warn_on_empty = FALSE
  )

  foo <- "abc"
  local_vcr_configure(filter_sensitive_data = list("<<foo>>" = foo))
  url <- "http://example.com"
  request <- vcr_request("get", url, "", list(api_key = foo))

  err <- UnhandledHTTPRequestError$new(request)
  cnd <- catch_cnd(err$construct_message(), classes = "error")
  expect_no_match(conditionMessage(cnd), foo)
})
