test_that("UnhandledHTTPRequestError fails well", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )
  request <- Request$new("post", hb("/post?a=5"), "", list(foo = "bar"))

  z <- UnhandledHTTPRequestError$new(request)
  expect_error(
    z$construct_message(),
    "There is currently no cassette in use"
  )

  # insert a cassette
  cas <- insert_cassette("asdsfd")
  withr::defer(eject_cassette())

  # # types
  expect_error(
    UnhandledHTTPRequestError$new(5),
    "request must be of class Request"
  )
})

test_that("UnhandledHTTPRequestError works as expected", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )
  request <- Request$new("post", hb("/post?a=5"), "", list(foo = "bar"))
  cas <- insert_cassette("turtle")
  withr::defer(eject_cassette())

  a <- UnhandledHTTPRequestError$new(request)

  expect_s3_class(a, "UnhandledHTTPRequestError")
  expect_s3_class(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "turtle")
  expect_type(a$construct_message, "closure")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
})

test_that("UnhandledHTTPRequestError works as expected", {
  ## API key in query param
  withr::local_envvar(
    FOO_BAR = "2k2k2k288gjrj2i21i",
    HELLO_WORLD = "asdfadfasfsfs239823n23"
  )
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_sensitive_data = list(
      "<<foo_bar_key>>" = Sys.getenv("FOO_BAR"),
      "<<hello_world_key>>" = Sys.getenv("HELLO_WORLD")
    ),
    warn_on_empty_cassette = FALSE
  )
  url <- paste0(
    hb("/get?api_key="),
    Sys.getenv("FOO_BAR"),
    "&other_secret=",
    Sys.getenv("HELLO_WORLD")
  )
  request <- Request$new("get", url, "")
  cas <- insert_cassette("bunny")
  withr::defer(eject_cassette())

  a <- UnhandledHTTPRequestError$new(request)

  expect_s3_class(a, "UnhandledHTTPRequestError")
  expect_s3_class(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "bunny")
  expect_type(a$construct_message, "closure")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
  expect_error(
    a$construct_message(),
    "<<foo_bar_key>>"
  )
  expect_error(
    a$construct_message(),
    "<<hello_world_key>>"
  )
})

test_that("UnhandledHTTPRequestError works as expected", {
  ## API key in header
  withr::local_envvar(FOO_BAR = "2k2k2k288gjrj2i21i")
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_sensitive_data = list("<<foo_bar_key>>" = Sys.getenv("FOO_BAR")),
    warn_on_empty_cassette = FALSE
  )
  url <- hb("/get")
  request <- Request$new("get", url, "", list(api_key = Sys.getenv("FOO_BAR")))
  cas <- insert_cassette(
    "frog",
    match_requests_on = c("method", "uri", "headers")
  )
  withr::defer(eject_cassette())

  a <- UnhandledHTTPRequestError$new(request)

  expect_s3_class(a, "UnhandledHTTPRequestError")
  expect_s3_class(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "frog")
  expect_type(a$construct_message, "closure")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
  expect_error(
    a$construct_message(),
    "api_key: <<foo_bar_key>>"
  )
})

test_that("UnhandledHTTPRequestError works as expected", {
  ## API key not found or empty (i.e., "")
  withr::local_envvar(HELLO_MARS = "asdfadfasfsfs239823n23")
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_sensitive_data = list(
      "<<bar_foo_key>>" = Sys.getenv("BAR_FOO"),
      "<<hello_mars_key>>" = Sys.getenv("HELLO_MARS")
    ),
    warn_on_empty_cassette = FALSE
  )
  url <- paste0(hb("/get?api_key="), Sys.getenv("HELLO_MARS"))
  request <- Request$new("get", url, "")
  cas <- insert_cassette("bunny2")
  withr::defer(eject_cassette())

  a <- UnhandledHTTPRequestError$new(request)

  expect_s3_class(a, "UnhandledHTTPRequestError")
  expect_s3_class(a$cassette, "Cassette")
  expect_equal(a$cassette$name, "bunny2")
  expect_type(a$construct_message, "closure")

  expect_error(
    a$construct_message(),
    "An HTTP request has been made that vcr does not know how to handle"
  )
  # we should see this string in the error message
  expect_error(
    a$construct_message(),
    "<<hello_mars_key>>"
  )
  # we should not see this string in the error message b/c it's empty/missing
  res <- tryCatch(a$construct_message(), error = function(e) e)
  expect_false(grepl("<<bar_foo_key>>", res$message))
})

# FIXME: use this test block when body matching implemented
## API key in body
# Sys.setenv(FOO_BAR = "asdfa9sdf899a0s8fa0f8a0sdfasdf")
# dir <- tempdir()
# invisible(
#   vcr_configure(dir = dir, filter_sensitive_data =
#     list("<<foo_bar_key>>" = Sys.getenv("FOO_BAR")))
# )
# url <- hb("/get")
# request <- Request$new("get", url, list(api_key = Sys.getenv("FOO_BAR")))
# unlink(file.path(vcr_c$dir, "alligator"))
# cas <- suppressMessages(insert_cassette("alligator",
#   match_requests_on = c("method", "uri", "body")))

# test_that("UnhandledHTTPRequestError works as expected", {
#   a <- UnhandledHTTPRequestError$new(request)

#   expect_s3_class(a, "UnhandledHTTPRequestError")
#   expect_s3_class(a$cassette, "Cassette")
#   expect_equal(a$cassette$name, "alligator")
#   expect_s3_class(a$construct_message, "function")

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
