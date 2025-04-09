skip_on_cran()

test_that("RequestHandlerHttr", {
  load("httr_obj.rda")

  expect_error(
    RequestHandlerHttr$new(),
    "\"request\" is missing",
    class = "error"
  )
})

test_that("RequestHandlerHttr: httr", {
  skip_if_not_installed("xml2")

  load("httr_obj.rda")
  x <- RequestHandlerHttr$new(httr_obj)

  expect_s3_class(x, "RequestHandlerHttr")
  expect_type(x$handle, "closure")
  expect_error(x$handle())

  # do request
  insert_cassette("greencow")
  response <- x$handle()

  expect_s3_class(response, "response")
  # status code is correct
  expect_equal(response$status_code, 404)

  eject_cassette("greencow")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "greencow.yml"))
})
