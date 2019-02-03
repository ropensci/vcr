library("httr")

vcr_configure(dir = tempdir())

context("httr: status code works")
test_that("httr status code works", {
  load("httr_obj.rda")

  expect_is(httr_obj, "request")

  x <- RequestHandlerHttr$new(httr_obj)

  expect_is(x, "RequestHandlerHttr")
  expect_is(x$handle, "function")
  expect_error(x$handle())

  # do request
  insert_cassette("foobar")
  response <- x$handle()

  expect_is(response, "response")
  # status code is correct
  expect_equal(response$status_code, 404)

  eject_cassette("foobar")

  # call again
  insert_cassette("foobar")
  response2 <- x$handle()

  expect_is(response2, "response")
  # status code is correct
  expect_equal(response2$status_code, 404)

  eject_cassette("foobar")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "foobar.yml"))
})


context("httr: use_cassette works")
test_that("httr use_cassette works", {
  out <- use_cassette("httr_test1", {
    x <- GET("https://httpbin.org/404")
  })

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr_test1")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # response
  expect_is(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, "https://httpbin.org/404")

  # response body
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  expect_is(str[[1]]$response$body$string, "character")
  expect_match(str[[1]]$response$body$string, "404")
  expect_match(str[[1]]$response$body$string, "DOCTYPE HTML")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr_test2.yml"))
})


context("httr: use_cassette w/ preserve_exact_body_bytes")
test_that("httr use_cassette works", {
  out <- use_cassette("httr_test2", {
    x <- GET("https://httpbin.org/404")
  }, preserve_exact_body_bytes = TRUE)

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr_test2")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # response
  expect_is(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, "https://httpbin.org/404")

  # response body
  str <- yaml::yaml.load_file(out$manfile)
  str <- rawToChar(base64enc::base64decode(
    str$http_interactions[[1]]$response$body$string))
  expect_is(str, "character")
  expect_match(str, "404")
  expect_match(str, "DOCTYPE HTML")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr_test2.yml"))
})
