vcr_configure(dir = tempdir())

test_that("RequestHandlerHttr", {
  load("httr_obj.rda")
  expect_is(RequestHandlerHttr, "R6ClassGenerator")

  expect_error(RequestHandlerHttr$new(),
    "\"request\" is missing", class = "error")
})

test_that("RequestHandlerHttr: httr", {
  skip_if_not_installed("xml2")

  load("httr_obj.rda")
  x <- RequestHandlerHttr$new(httr_obj)

  expect_is(x, "RequestHandlerHttr")
  expect_is(x$handle, "function")
  expect_error(x$handle())

  # do request
  insert_cassette("greencow")
  response <- x$handle()

  expect_is(response, "response")
  # status code is correct
  expect_equal(response$status_code, 404)

  eject_cassette("greencow")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "greencow.yml"))
})
