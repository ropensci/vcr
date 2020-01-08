context("Cassette")

test_that("Cassette", {
  expect_is(Cassette, "R6ClassGenerator")
  cl <- Cassette$new(name = "stuff")
  expect_is(cl,  "R6")
  expect_is(cl,  "Cassette")

  # eject cassette
  cl$eject()
})

test_that("Cassette fails well", {
  expect_error(Cassette$new(), "\"name\" is missing")
})

test_that("Cassette fails well with invalid record mode", {
  expect_error(
    Cassette$new(name = "stuff2", record = "asdfadfs"),
    "'record' value of 'asdfadfs' is not in the allowed set"
  )
})

test_that("Cassette fails well with invalid request matchers", {
  expect_error(
    Cassette$new(name = "stuff2", match_requests_on = "x"),
    "1 or more 'match_requests_on' values \\(x\\) is not in the allowed set"
  )
})

test_that("Cassette fails well with unsupported matcher", {
  expect_error(Cassette$new("foobar89", match_requests_on = "host"),
    "we do not yet support host and path matchers")
})


test_that("make_http_interaction works as expected", {
  #### Prepare http responses
  # crul_resp1 <- crul::HttpClient$new("https://httpbin.org/get?foo=bar")$get()
  # save(crul_resp1, file = "tests/testthat/crul_resp1.rda", version = 2)

  # crul_resp2 <- crul::HttpClient$new("https://httpbin.org/image/png")$get()
  # save(crul_resp2, file = "tests/testthat/crul_resp2.rda", version = 2)

  # httr_resp1 <- httr::GET("https://httpbin.org/get?foo=bar")
  # save(httr_resp1, file = "tests/testthat/httr_resp1.rda", version = 2)

  # httr_resp2 <- httr::GET("https://httpbin.org/image/png")
  # save(httr_resp2, file = "tests/testthat/httr_resp2.rda", version = 2)
  
  # make a cassettes
  zz <- Cassette$new(name = "bluecheese")

  # crul, with non-image response body
  # $response$body should be class `character`
  load("crul_resp1.rda")
  aa <- zz$make_http_interaction(crul_resp1)
  expect_is(aa, "HTTPInteraction")
  expect_is(aa$request, "Request")
  expect_is(aa$response, "VcrResponse")
  expect_is(aa$response$body, "character")

  # crul, with image response body
  # $response$body should be class `raw`
  load("crul_resp2.rda")
  bb <- zz$make_http_interaction(crul_resp2)
  expect_is(bb, "HTTPInteraction")
  expect_is(bb$request, "Request")
  expect_is(bb$response, "VcrResponse")
  expect_is(bb$response$body, "raw")

  # eject cassette
  zz$eject()
})


# cleanup
unlink(file.path(vcr_configuration()$dir, "stuff.yml"))
unlink(file.path(vcr_configuration()$dir, "stuff2.yml"))
unlink(file.path(vcr_configuration()$dir, "foobar89.yml"))
unlink(file.path(vcr_configuration()$dir, "bluecheese.yml"))
