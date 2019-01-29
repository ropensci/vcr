context("UnhandledHTTPRequestError")

dir <- tempdir()
invisible(vcr_configure(dir = dir))

request <- Request$new(
  "post", "https://eu.httpbin.org/post?a=5", "", list(foo = "bar"))

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


context("UnhandledHTTPRequestError: secret handling")

## API key in query param
Sys.setenv(FOO_BAR = "2k2k2k288gjrj2i21i")
dir <- tempdir()
invisible(
  vcr_configure(dir = dir, filter_sensitive_data = 
    list("<<foo_bar_key>>" = Sys.getenv("FOO_BAR")))
)
url <- paste0("https://eu.httpbin.org/get?api_key=", Sys.getenv("FOO_BAR"))
request <- Request$new("get", url, "")
unlink(file.path(vcr_c$dir, "bunny"))
cas <- suppressMessages(insert_cassette("bunny"))

test_that("UnhandledHTTPRequestError works as expected", {
  a <- UnhandledHTTPRequestError$new(request)

  expect_is(a, "UnhandledHTTPRequestError")
  expect_is(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "bunny")
  expect_is(a$construct_message, "function")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
  expect_error(
    a$construct_message(),
    "<<foo_bar_key>>"
  )
})
# cleanup
eject_cassette()
unlink(file.path(vcr_configuration()$dir, "bunny.yml"))
# reset configuration
vcr_configure_reset()


## API key in header
Sys.setenv(FOO_BAR = "2k2k2k288gjrj2i21i")
dir <- tempdir()
invisible(
  vcr_configure(dir = dir, filter_sensitive_data =
    list("<<foo_bar_key>>" = Sys.getenv("FOO_BAR")))
)
url <- "https://eu.httpbin.org/get"
request <- Request$new("get", url, "", list(api_key = Sys.getenv("FOO_BAR")))
unlink(file.path(vcr_c$dir, "frog"))
cas <- suppressMessages(insert_cassette("frog",
  match_requests_on = c("method", "uri", "headers")))

test_that("UnhandledHTTPRequestError works as expected", {
  a <- UnhandledHTTPRequestError$new(request)

  expect_is(a, "UnhandledHTTPRequestError")
  expect_is(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "frog")
  expect_is(a$construct_message, "function")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
  expect_error(
    a$construct_message(),
    "api_key: <<foo_bar_key>>"
  )
})
# cleanup
eject_cassette()
unlink(file.path(vcr_configuration()$dir, "frog.yml"))
# reset configuration
vcr_configure_reset()





# FIXME: use this test block when body matching implemented
## API key in body
# Sys.setenv(FOO_BAR = "asdfa9sdf899a0s8fa0f8a0sdfasdf")
# dir <- tempdir()
# invisible(
#   vcr_configure(dir = dir, filter_sensitive_data =
#     list("<<foo_bar_key>>" = Sys.getenv("FOO_BAR")))
# )
# url <- "https://eu.httpbin.org/get"
# request <- Request$new("get", url, list(api_key = Sys.getenv("FOO_BAR")))
# unlink(file.path(vcr_c$dir, "alligator"))
# cas <- suppressMessages(insert_cassette("alligator",
#   match_requests_on = c("method", "uri", "body")))

# test_that("UnhandledHTTPRequestError works as expected", {
#   a <- UnhandledHTTPRequestError$new(request)

#   expect_is(a, "UnhandledHTTPRequestError")
#   expect_is(a$cassette, "Cassette")
#   expect_equal(a$cassette$name, "alligator")
#   expect_is(a$construct_message, "function")

#   expect_error(
#     a$construct_message(),
#     "An HTTP request has been made that vcr does not know how to handle"
#   )
#   expect_error(
#     a$construct_message(),
#     "api_key: <<foo_bar_key>>"
#   )
# })
# # cleanup
# eject_cassette()
# unlink(file.path(vcr_configuration()$dir, "alligator.yml"))
# # reset configuration
# vcr_configure_reset()
