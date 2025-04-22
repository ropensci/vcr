test_that("HttpInteraction", {
  crul::mock(FALSE)

  url <- hb("/post")
  body <- list(foo = "bar")
  cli <- crul::HttpClient$new(url = url)
  res <- cli$post(body = body)

  # request
  request <- Request$new(
    "POST",
    uri = url,
    body = body,
    headers = res$response_headers
  )
  # response
  response <- VcrResponse$new(
    res$status_http(),
    res$response_headers,
    res$parse("UTF-8")
  )

  x <- HTTPInteraction$new(request = request, response = response)

  # timestamp
  expect_s3_class(x, "HTTPInteraction")
  expect_s3_class(x$recorded_at, 'POSIXct')
  expect_type(x$recorded_at, 'double')

  # methods and objects
  expect_s3_class(x$request, "Request")
  expect_s3_class(x$response, "VcrResponse")
})
