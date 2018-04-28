context("UnhandledHTTPRequestError")

dir <- tempdir()
invisible(vcr_configure(dir = dir))

request <- Request$new(
  "post", 'https://eu.httpbin.org/post?a=5', "", list(foo = "bar"))

test_that("UnhandledHTTPRequestError fails well", {
  z <- UnhandledHTTPRequestError$new(request)
  expect_error(
    z$construct_message(),
    "There is currently no cassette in use"
  )

  # insert  a cassette
  unlink(file.path(vcr_c$dir, "turtle"))
  cas <- suppressMessages(insert_cassette("turtle"))

  # types
  expect_error(
    UnhandledHTTPRequestError$new(5),
    "request must be of class Request"
  )

  # types
  expect_error(
    UnhandledHTTPRequestError$new(request, 5),
    "cassette must be of class character"
  )
})

test_that("UnhandledHTTPRequestError works as expected", {
  a <- UnhandledHTTPRequestError$new(request)

  expect_is(a, "UnhandledHTTPRequestError")
  expect_is(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "turtle")
  expect_is(a$construct_message, "function")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
})

# cleanup
eject_cassette()
unlink(file.path(vcr_configuration()$dir, "turtle.yml"))

# reset configuration
vcr_configure_reset()
