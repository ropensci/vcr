test_that('request matching is not sensitive to escaping special characters', {
  local_vcr_configure(dir = withr::local_tempdir())
  url <- hb("/get?update=2022-01-01T00:00:00&p2=ok")

  # curl does not escape
  aa <- use_cassette('test', res <- crul::HttpClient$new(url)$get())
  expect_true(res$status_code == 200)

  # httr does escape
  bb <- use_cassette('test', res <- httr::GET(url))
  expect_true(res$status_code == 200)
})
