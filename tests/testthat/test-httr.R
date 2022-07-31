skip_on_cran()

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
  invisible(use_cassette("httr_test1", {
    x2 <- GET("https://httpbin.org/404")
  }))

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr_test1")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # request - 1st http call
  expect_is(x$request, "request")
  expect_equal(x$request$method, "GET")
  expect_equal(x$request$url, "https://httpbin.org/404")
  expect_named(x$request$headers, "Accept")
  expect_null(x$request$fields)
  expect_true(x$request$options$httpget)
  expect_is(x$request$output, "write_function")
  
  # request - 2nd http call
  expect_is(x2$request, "request")
  expect_equal(x2$request$method, "GET")
  expect_equal(x2$request$url, "https://httpbin.org/404")
  expect_named(x2$request$headers, "Accept")
  expect_null(x2$request$fields)
  expect_true(x2$request$options$httpget)
  expect_null(x2$request$output) # can't really populate this from cassette

  # response
  expect_is(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, "https://httpbin.org/404")
  expect_output(print(x), "Not Found")
  expect_output(print(x), "HTML PUBLIC")

  # response body
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  expect_is(str[[1]]$response$body$string, "character")
  expect_match(str[[1]]$response$body$string, "404")
  expect_match(str[[1]]$response$body$string, "DOCTYPE HTML")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr_test1.yml"))
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
    str$http_interactions[[1]]$response$body$base64_string))
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

context("adapter-httr: use_cassette w/ simple auth")
test_that("httr works with simple auth and hides auth details", {

  use_cassette("httr_test_simple_auth", {
    x <- GET("https://httpbin.org/basic-auth/foo/bar",
      authenticate("foo", "bar"))
  })

  # successful request
  expect_equal(x$status_code, 200)

  # no auth details in the cassette
  path <- file.path(vcr_configuration()$dir, "httr_test_simple_auth.yml")
  chars <- paste0(readLines(path), collapse = "")
  yml <- yaml::yaml.load_file(path)

  expect_false(grepl("Authorization", chars))
  expect_false("Authorization" %in% names(yml$http_interactions[[1]]$request$headers))

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr_test_simple_auth.yml"))
})

context("adapter-httr: POST requests works")
test_that("httr POST requests works", {
  # body type: named list
  out <- use_cassette("httr_post_named_list", {
    x <- POST("https://httpbin.org/post", body = list(foo = "bar"))
  })
  expect_false(out$is_empty())
  expect_is(x, "response")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$form, list(foo = "bar"))

  # body type: character
  out2 <- use_cassette("httr_post_string", {
    z <- POST("https://httpbin.org/post", body = "some string")
  })
  expect_false(out2$is_empty())
  expect_is(z, "response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: raw
  out3 <- use_cassette("httr_post_raw", {
    z <- POST("https://httpbin.org/post", body = charToRaw("some string"))
  })
  expect_false(out3$is_empty())
  expect_is(z, "response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: upload_file
  ff <- tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette("httr_post_upload_file", {
    b <- POST("https://httpbin.org/post",
      body = list(y = httr::upload_file(ff)))
  })
  expect_false(out4$is_empty())
  expect_is(b, "response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$y, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty
  unlink(ff)
  
  ## upload_file not in a list
  # out6 <- use_cassette("httr_post_upload_file_no_list", {
  #   d <- POST("https://httpbin.org/post",
  #     body = httr::upload_file(system.file("CITATION")))
  # })
  # expect_false(out6$is_empty())
  # expect_is(d, "response")
  # expect_equal(d$status_code, 200)
  # str <- yaml::yaml.load_file(out6$manfile)$http_interactions
  # strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # expect_equal(length(strj$files), 0) # files empty
  # expect_match(strj$data, "bibentry\\(") # data not empty

  # body type: NULL
  out5 <- use_cassette("httr_post_null", {
    m <- POST("https://httpbin.org/post", body = NULL)
  })
  expect_false(out5$is_empty())
  expect_is(m, "response")
  expect_equal(m$status_code, 200)
  str <- yaml::yaml.load_file(out5$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr_post_named_list.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr_post_string.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr_post_raw.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr_post_upload_file.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr_post_null.yml"))
})
