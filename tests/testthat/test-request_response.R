test_that("request_summary works", {
  request <- vcr_request(
    "POST",
    "http://example.com",
    "body",
    list(x = 1, y = 2)
  )

  expect_snapshot({
    cat(request_summary(request, c('method', 'uri')))
    cat(request_summary(request, c('method', 'uri', 'body')))
    cat(request_summary(request, c('method', 'uri', 'headers')))
    cat(request_summary(request, c('method', 'uri', 'body', 'headers')))
  })
})

test_that("response_summary works", {
  response <- vcr_response(200, body = strrep("body", 100))
  expect_snapshot(response_summary(response))
})

test_that("response_summary works with raw body", {
  response <- vcr_response(200, body = charToRaw("body"))
  expect_snapshot(response_summary(response))
})

test_that("response_summary - handles bad multibyte characters by changing encoding", {
  skip_on_cran()

  # res <- crul::HttpClient$new("https://google.com")$get()
  # google_response <- rawToChar(res$content)
  # save(google_response, file = "tests/testthat/google_response.rda", version = 2L)
  load("google_response.rda")
  response <- vcr_response(200, as.list(google_response))
  expect_snapshot(response_summary(response))
})
