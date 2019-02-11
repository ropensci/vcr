library("httr")

vcr_configure(dir = tempdir())

context("adapter-httr: status code works")
test_that("httr status code works", {
  skip_if_not_installed("xml2")

  load("httr_obj.rda")

  expect_is(httr_obj, "request")

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

  # call again
  insert_cassette("greencow")
  response2 <- x$handle()

  expect_is(response2, "response")
  # status code is correct
  expect_equal(response2$status_code, 404)

  eject_cassette("greencow")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "greencow.yml"))
})


context("adapter-httr: use_cassette works")
test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")

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


context("adapter-httr: use_cassette w/ preserve_exact_body_bytes")
test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")

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


context("adapter-httr: use_cassette w/ >1 request per cassette")
test_that("httr w/ >1 request per cassette", {
  skip_if_not_installed("xml2")

  out <- use_cassette("multiple_queries_httr_record_once", {
    x404 <- GET("https://httpbin.org/status/404")
    x500 <- GET("https://httpbin.org/status/500")
    x418 <- GET("https://httpbin.org/status/418")

    expect_equal(status_code(x404), 404)
    expect_equal(status_code(x500), 500)
    expect_equal(status_code(x418), 418)
  })

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "multiple_queries_httr_record_once")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # response
  expect_is(x404, "response")
  expect_equal(x404$status_code, 404)
  expect_is(x500, "response")
  expect_equal(x500$status_code, 500)
  expect_is(x418, "response")
  expect_equal(x418$status_code, 418)

  # response body
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  expect_is(str, "list")
  expect_is(str[[3]], "list")
  expect_match(str[[3]]$request$uri , "418")
  expect_match(str[[3]]$response$body$string, "teapot")

  # cleanup
  unlink(file.path(vcr_configuration()$dir,
    "multiple_queries_httr_record_once.yml"))
})
