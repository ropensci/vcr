test_that("vcr_last_error fails well", {
  the$last_error <- list()
  expect_error(vcr_last_error(), "no error to report")

  # insert  a cassette - same result
  local_cassette("rabbit", warn_on_empty = FALSE)
  expect_error(vcr_last_error(), "no error to report")
})

test_that("vcr_last_error works: no cassette in use yet", {
  request <- vcr_request("post", hb("/post?a=5"), "", list(foo = "bar"))
  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(vcr_last_error(), "There is currently no cassette in use")
  expect_message(vcr_last_error(), "If you want vcr to record this request")
  expect_message(vcr_last_error(), "http-testing/debugging-")
})

test_that("vcr_last_error works: cassette in use", {
  request <- vcr_request("post", hb("/post?a=5"), "", list(foo = "bar"))

  local_cassette("bunny", warn_on_empty = FALSE)
  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(
    vcr_last_error(),
    "vcr is currently using the following cassette"
  )
  expect_message(vcr_last_error(), "Under the current configuration")
  expect_message(vcr_last_error(), "new_episodes")
})
