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
    res$parse("UTF-8"),
    res$response_headers$status
  )

  x <- HTTPInteraction$new(request = request, response = response)

  # timestamp
  expect_s3_class(x, "HTTPInteraction")
  expect_s3_class(x$recorded_at, 'POSIXct')
  expect_type(x$recorded_at, 'double')

  # methods and objects
  expect_type(x$to_hash, "closure")
  expect_type(x$from_hash, "closure")
  expect_s3_class(x$request, "Request")
  expect_s3_class(x$response, "VcrResponse")

  # to_hash method
  expect_type(x$to_hash(), "list")
  expect_named(x$to_hash(), c('request', 'response', 'recorded_at'))

  # make an HTTPInteraction from a hash with the object already made
  expect_s3_class(x$from_hash(x$to_hash()), "HTTPInteraction")

  # Make an HTTPInteraction from a hash alone
  my_hash <- x$to_hash()
  expect_s3_class(HTTPInteraction$new()$from_hash(my_hash), "HTTPInteraction")
})
