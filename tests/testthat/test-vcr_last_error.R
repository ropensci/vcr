context("vcr_last_error")

dir <- tempdir()
invisible(vcr_configure(dir = dir, warn_on_empty_cassette = FALSE))
vcr__env$last_error <- list()

test_that("vcr_last_error fails well", {
  expect_error(vcr_last_error(), "no error to report")

  # insert  a cassette - same result
  unlink(file.path(vcr_c$dir, "rabbit"))
  cas <- suppressMessages(insert_cassette("rabbit"))

  expect_error(vcr_last_error(), "no error to report")
  
  # cleanup before next test block
  eject_cassette()
  unlink(file.path(vcr_configuration()$dir, "rabbit.yml"))
})

# cleanup
vcr_configure_reset()

# setup for next block
dir <- tempdir()
invisible(vcr_configure(dir = dir))
request <- Request$new(
  "post", hb("/post?a=5"), "", list(foo = "bar"))

test_that("vcr_last_error works: no casssette in use yet", {
  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(vcr_last_error(), "There is currently no cassette in use")
  expect_message(vcr_last_error(), "If you want vcr to record this request")
  expect_message(vcr_last_error(), "http-testing/debugging-")
})

test_that("vcr_last_error works: casssette in use", {
  unlink(file.path(vcr_c$dir, "bunny"))
  cas <- suppressMessages(insert_cassette("bunny"))

  err <- UnhandledHTTPRequestError$new(request)
  expect_error(err$construct_message())
  expect_message(vcr_last_error(), "vcr is currently using the following cassette")
  expect_message(vcr_last_error(), "Under the current configuration")
  expect_message(vcr_last_error(), "new_episodes")
})

# reset configuration
suppressWarnings(eject_cassette())
unlink(file.path(vcr_configuration()$dir, "bunny.yml"))
vcr_configure_reset()
