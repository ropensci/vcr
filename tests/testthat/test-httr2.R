skip_on_cran()

test_that("httr2 status code works", {
  local_vcr_configure(dir = withr::local_tempdir())

  # httr2_obj <- request(hb("/getttttt"))
  # save(httr2_obj, file="tests/testthat/httr2_obj.rda", version = 2L)
  load("httr2_obj.rda")

  expect_s3_class(httr2_obj, "httr2_request")

  x <- RequestHandlerHttr2$new(httr2_obj)

  expect_s3_class(x, "RequestHandlerHttr2")
  expect_type(x$handle, "closure")
  expect_error(x$handle())

  # do request
  insert_cassette("bluecow")
  the_response <- x$handle()
  # the_response <- httr2::last_response()

  expect_s3_class(the_response, "httr2_response")
  # status code is correct
  expect_equal(the_response$status_code, 404)

  eject_cassette()

  # call again
  insert_cassette("bluecow")
  x$handle()
  response2 <- httr2::last_response()

  expect_s3_class(response2, "httr2_response")
  # status code is correct
  expect_equal(response2$status_code, 404)

  eject_cassette()
})

test_that("httr2 use_cassette works", {
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette("httr2_test1", {
    x <- httr2::request(hb_remote("/get")) |> httr2::req_perform()
  })
  invisible(use_cassette("httr2_test1", {
    x2 <- httr2::request(hb_remote("/get")) |> httr2::req_perform()
  }))

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr2_test1")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # request - 1st http call
  expect_s3_class(x$request, "httr2_request")
  expect_equal(x$request$method, "GET")
  expect_equal(x$request$url, hb_remote("/get"))
  expect_named(x$request$headers, NULL)
  expect_type(x$request$fields, "list")

  # request - 2nd http call
  expect_s3_class(x2$request, "httr2_request")
  expect_equal(x2$request$method, "GET")
  expect_equal(x2$request$url, hb_remote("/get"))
  expect_named(x2$request$headers, NULL)
  expect_null(x2$request$fields)

  # response - stuff
  expect_s3_class(x, "httr2_response")
  expect_equal(x$status_code, 200)
  expect_equal(x$url, hb_remote("/get"))

  # fixture file
  str <- yaml::yaml.load_file(out$file())$http_interactions
  expect_type(str[[1]]$response$body$string, "character")
  expect_match(str[[1]]$response$body$string, "headers")
  expect_match(str[[1]]$response$body$string, "libcurl")
})

test_that("httr2 use_cassette works", {
  local_vcr_configure(dir = withr::local_tempdir())
  out <- use_cassette(
    "httr2_test2",
    {
      x <- httr2::request(hb_remote("/get")) |> httr2::req_perform()
    },

    preserve_exact_body_bytes = TRUE
  )

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr2_test2")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # response
  expect_s3_class(x, "httr2_response")
  expect_equal(x$status_code, 200)
  expect_equal(x$url, hb_remote("/get"))

  # response body
  str <- yaml::yaml.load_file(out$file())
  str <- rawToChar(jsonlite::base64_dec(
    str$http_interactions[[1]]$response$body$base64_string
  ))
  expect_type(str, "character")
  expect_match(str, "Connection")
  expect_match(str, "httpbin")
})

test_that("httr2 w/ req_error", {
  local_vcr_configure(dir = withr::local_tempdir())

  out <- use_cassette("httr2_errors_modify_with_req_error", {
    x404 <- httr2::request(hb("/status/404")) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("httr2_errors_modify_with_req_error", {
    x404 <- httr2::request(hb("/status/404")) |>
      httr2::req_error(is_error = function(resp) FALSE) |>
      httr2::req_perform()
  })

  expect_equal(x404$status_code, 404)

  # cassette
  expect_s3_class(out, "Cassette")
  expect_match(out$file(), "httr2_errors_modify_with_req_error")
  expect_false(out$is_empty())
  expect_s3_class(out$recorded_at, "POSIXct")

  # response
  expect_s3_class(x404, "httr2_response")
  expect_equal(x404$status_code, 404)

  # response body
  str <- yaml::yaml.load_file(out$file())$http_interactions
  expect_type(str, "list")
  expect_type(str[[1]], "list")
  expect_match(str[[1]]$request$uri, "404")
})

test_that("httr2 error", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("httr2_errors_catch_error", {
    expect_error(
      httr2::request(hb("/status/404")) |> httr2::req_perform()
    )
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("httr2_errors_catch_error", {
    expect_error(
      httr2::request(hb("/status/404")) |> httr2::req_perform()
    )
  })
})

test_that("httr2 w/ multiple errors per cassette", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("multiple_errors_per_cassette", {
    expect_error(httr2::request(hb("/status/404")) |> httr2::req_perform())
    expect_error(httr2::request(hb("/status/500")) |> httr2::req_perform())
    expect_error(httr2::request(hb("/status/418")) |> httr2::req_perform())
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("multiple_errors_per_cassette", {
    expect_error(httr2::request(hb("/status/404")) |> httr2::req_perform())
    expect_error(httr2::request(hb("/status/500")) |> httr2::req_perform())
    expect_error(httr2::request(hb("/status/418")) |> httr2::req_perform())
  })
})

## httr removes the header, but with httr2 we have to explicity remove it
test_that("httr2 works with simple auth and hides auth details", {
  local_vcr_configure(dir = withr::local_tempdir())
  # Authorization header IS in the cassette after filtering
  use_cassette("httr2_test_simple_auth_no_filter", {
    x <- httr2::request(hb("/basic-auth/foo/bar")) |>
      httr2::req_auth_basic("foo", "bar") |>
      httr2::req_perform()
  })

  yml <- read_cassette("httr2_test_simple_auth_no_filter.yml")
  expect_true(
    "Authorization" %in% names(yml$http_interactions[[1]]$request$headers)
  )

  # Authorization header IS NOT in the cassette after filtering
  local_vcr_configure(filter_request_headers = "Authorization")
  use_cassette("httr2_test_simple_auth_yes_filter", {
    x <- httr2::request(hb("/basic-auth/foo/bar")) |>
      httr2::req_auth_basic("foo", "bar") |>
      httr2::req_perform()
  })
  yml <- read_cassette("httr2_test_simple_auth_yes_filter.yml")
  expect_false(
    "Authorization" %in% names(yml$http_interactions[[1]]$request$headers)
  )
})

test_that("httr2 POST requests works", {
  local_vcr_configure(dir = withr::local_tempdir())

  # body type: named list
  out <- use_cassette("httr2_post_named_list", {
    x <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_json(list(foo = "bar")) |>
      httr2::req_perform()
  })
  expect_false(out$is_empty())
  expect_s3_class(x, "httr2_response")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$file())$http_interactions
  # request body
  expect_equal(
    "{\"foo\":\"bar\"}",
    str[[1]]$request$body$string
  )
  # response body
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "{\"foo\":\"bar\"}")

  # body type: character
  out2 <- use_cassette("httr2_post_string", {
    z <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_raw("some string") |>
      httr2::req_perform()
  })
  expect_false(out2$is_empty())
  expect_s3_class(z, "httr2_response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$file())$http_interactions
  # request body
  expect_equal(
    "some string",
    str[[1]]$request$body$string
  )
  # response body
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: raw
  out3 <- use_cassette("httr2_post_raw", {
    z <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_raw(charToRaw("some string")) |>
      httr2::req_perform()
    # z <- POST(hb("/post"), body = charToRaw("some string"))
  })
  expect_false(out3$is_empty())
  expect_s3_class(z, "httr2_response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$file())$http_interactions
  # request body
  expect_equal(
    "some string",
    str[[1]]$request$body$string
  )
  # response body
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # file with req_body_file
  ff <- withr::local_tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette("httr2_post_body_file", {
    b <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_file(ff) |>
      httr2::req_perform()
  })
  expect_false(out4$is_empty())
  expect_s3_class(b, "httr2_response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(length(strj$files$y), 0) # files empty
  expect_match(strj$data, "hello world") # data not empty

  # using multipart
  gg <- withr::local_tempfile(fileext = ".txt")
  cat("hello world\n", file = gg)
  out4 <- use_cassette("httr2_post_body_multipart", {
    b <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_multipart(a = curl::form_file(gg), b = "some data") |>
      httr2::req_perform()
  })
  expect_false(out4$is_empty())
  expect_s3_class(b, "httr2_response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$a, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty

  # body type: NULL
  out5 <- use_cassette("httr2_post_null", {
    m <- httr2::request(hb_remote("/post")) |>
      httr2::req_body_raw("") |>
      httr2::req_perform()
  })
  expect_false(out5$is_empty())
  expect_s3_class(m, "httr2_response")
  expect_equal(m$status_code, 200)
  str <- yaml::yaml.load_file(out5$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")
})
