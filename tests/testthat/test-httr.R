skip_on_cran()

test_that("httr status code works", {
  local_vcr_configure(dir = withr::local_tempdir())
  skip_if_not_installed("xml2")

  load("httr_obj.rda")

  expect_s3_class(httr_obj, "request")

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
  eject_cassette()

  # call again
  insert_cassette("greencow")
  response2 <- x$handle()

  expect_s3_class(response2, "response")
  # status code is correct
  expect_equal(response2$status_code, 404)
  eject_cassette()
})

test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette(
    "httr_test1",
    x <- httr::GET(hb_remote("/404"))
  )
  invisible(use_cassette(
    "httr_test1",
    x2 <- httr::GET(hb_remote("/404"))
  ))

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr_test1")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # request - 1st http call
  expect_s3_class(x$request, "request")
  expect_equal(x$request$method, "GET")
  expect_equal(x$request$url, hb_remote("/404"))
  expect_named(x$request$headers, "Accept")
  expect_null(x$request$fields)
  expect_true(x$request$options$httpget)
  expect_s3_class(x$request$output, "write_function")

  # request - 2nd http call
  expect_s3_class(x2$request, "request")
  expect_equal(x2$request$method, "GET")
  expect_equal(x2$request$url, hb_remote("/404"))
  expect_named(x2$request$headers, "Accept")
  expect_null(x2$request$fields)
  expect_true(x2$request$options$httpget)
  expect_null(x2$request$output) # can't really populate this from cassette

  # response
  expect_s3_class(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, hb_remote("/404"))
  expect_output(print(x), "Not Found")
  expect_output(print(x), "HTML PUBLIC")

  # response body
  str <- yaml::yaml.load_file(out$file())$http_interactions
  expect_type(str[[1]]$response$body$string, "character")
  expect_match(str[[1]]$response$body$string, "404")
  expect_match(str[[1]]$response$body$string, "DOCTYPE HTML")
})

test_that("httr use_cassette works", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette(
    "httr_test2",
    x <- httr::GET(hb_remote("/404")),
    preserve_exact_body_bytes = TRUE
  )

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr_test2")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # response
  expect_s3_class(x, "response")
  expect_equal(x$status_code, 404)
  expect_equal(x$url, hb_remote("/404"))

  # response body
  str <- yaml::yaml.load_file(out$file())
  str <- rawToChar(jsonlite::base64_dec(
    str$http_interactions[[1]]$response$body$base64_string
  ))
  expect_type(str, "character")
  expect_match(str, "404")
  expect_match(str, "DOCTYPE HTML")
})

test_that("httr w/ >1 request per cassette", {
  skip_if_not_installed("xml2")
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette("multiple_queries_httr_record_once", {
    x404 <- httr::GET(hb("/status/404"))
    x500 <- httr::GET(hb("/status/500"))
    x418 <- httr::GET(hb("/status/418"))

    expect_equal(httr::status_code(x404), 404)
    expect_equal(httr::status_code(x500), 500)
    expect_equal(httr::status_code(x418), 418)
  })

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "multiple_queries_httr_record_once")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # response
  expect_s3_class(x404, "response")
  expect_equal(x404$status_code, 404)
  expect_s3_class(x500, "response")
  expect_equal(x500$status_code, 500)
  expect_s3_class(x418, "response")
  expect_equal(x418$status_code, 418)

  # response body
  str <- yaml::yaml.load_file(out$file())$http_interactions
  expect_type(str, "list")
  expect_type(str[[3]], "list")
  expect_match(str[[3]]$request$uri, "418")
  expect_match(str[[3]]$response$body$string, "teapot")
})

test_that("httr works with simple auth and hides auth details", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette(
    "httr_test_simple_auth",
    x <- httr::GET(hb("/basic-auth/foo/bar"), httr::authenticate("foo", "bar"))
  )
  # successful request
  expect_equal(x$status_code, 200)

  # no auth details in the cassette
  yml <- read_cassette("httr_test_simple_auth.yml")
  expect_false(
    "Authorization" %in% names(yml$http_interactions[[1]]$request$headers)
  )
})

test_that("httr POST requests works", {
  local_vcr_configure(dir = withr::local_tempdir())

  # body type: named list
  out <- use_cassette(
    "httr_post_named_list",
    x <- httr::POST(hb_remote("/post"), body = list(foo = "bar"))
  )
  expect_false(out$is_empty())
  expect_s3_class(x, "response")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$form, list(foo = "bar"))

  # body type: character
  out2 <- use_cassette(
    "httr_post_string",
    z <- httr::POST(hb_remote("/post"), body = "some string")
  )
  expect_false(out2$is_empty())
  expect_s3_class(z, "response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: raw
  out3 <- use_cassette(
    "httr_post_raw",
    z <- httr::POST(hb_remote("/post"), body = charToRaw("some string"))
  )
  expect_false(out3$is_empty())
  expect_s3_class(z, "response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: upload_file
  ff <- withr::local_tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette(
    "httr_post_upload_file",
    b <- httr::POST(hb_remote("/post"), body = list(y = httr::upload_file(ff)))
  )
  expect_false(out4$is_empty())
  expect_s3_class(b, "response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$y, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty

  ## upload_file not in a list
  # out6 <- use_cassette("httr_post_upload_file_no_list", {
  #   d <- POST(hb_remote("/post"),
  #     body = httr::upload_file(system.file("CITATION")))
  # })
  # expect_false(out6$is_empty())
  # expect_s3_class(d, "response")
  # expect_equal(d$status_code, 200)
  # str <- yaml::yaml.load_file(out6$file())$http_interactions
  # strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # expect_equal(length(strj$files), 0) # files empty
  # expect_match(strj$data, "bibentry\\(") # data not empty

  # body type: NULL
  out5 <- use_cassette(
    "httr_post_null",
    m <- httr::POST(hb_remote("/post"), body = NULL)
  )
  expect_false(out5$is_empty())
  expect_s3_class(m, "response")
  expect_equal(m$status_code, 200)
  str <- yaml::yaml.load_file(out5$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")
})
