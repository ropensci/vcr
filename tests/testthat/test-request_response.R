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
  response_raw <- vcr_response(200, body = charToRaw("body"))
  response_unknown <- vcr_response(200, body = strrep("body", 100))
  response_json <- vcr_response(
    200,
    body = strrep("body", 100),
    headers = list(`content-type` = "application/json")
  )

  expect_snapshot({
    response_summary(response_raw)
    response_summary(response_unknown)
    response_summary(response_json)
  })
})
