test_that("request_summary works", {
  request <- Request$new(
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
  response <- VcrResponse$new(200, list(), strrep("body", 100))
  expect_snapshot(response_summary(response))
})

test_that("response_summary works with raw body", {
  response <- VcrResponse$new(200, list(), charToRaw("body"))
  expect_snapshot(response_summary(response))
})

test_that("response_summary - handles bad multibyte characters by changing encoding", {
  skip_on_cran()

  # res <- crul::HttpClient$new("https://google.com")$get()
  # google_response <- rawToChar(res$content)
  # save(google_response, file = "tests/testthat/google_response.rda", version = 2L)
  load("google_response.rda")
  response <- VcrResponse$new(200, google_response)
  expect_snapshot(response_summary(response))
})
