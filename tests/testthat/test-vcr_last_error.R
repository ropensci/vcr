test_that("vcr_last_error fails well", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )
  vcr__env$last_error <- list()

  expect_error(vcr_last_error(), "no error to report")

  # insert  a cassette - same result
  cas <- suppressMessages(insert_cassette("rabbit"))
  withr::defer(eject_cassette())
  expect_error(vcr_last_error(), "no error to report")
})

test_that("vcr_last_error works: no casssette in use yet", {
  local_vcr_configure(dir = withr::local_tempdir())

  request <- Request$new("post", hb("/post?a=5"), "", list(foo = "bar"))
  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(vcr_last_error(), "There is currently no cassette in use")
  expect_message(vcr_last_error(), "If you want vcr to record this request")
  expect_message(vcr_last_error(), "http-testing/debugging-")
})

test_that("vcr_last_error works: casssette in use", {
  local_vcr_configure(dir = withr::local_tempdir())
  request <- Request$new("post", hb("/post?a=5"), "", list(foo = "bar"))

  cas <- insert_cassette("bunny")
  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(
    vcr_last_error(),
    "vcr is currently using the following cassette"
  )
  expect_message(vcr_last_error(), "Under the current configuration")
  expect_message(vcr_last_error(), "new_episodes")
})
