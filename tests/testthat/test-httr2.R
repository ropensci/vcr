skip_on_cran()

library("httr2")
vcr_configure(dir = tempdir())

context("adapter-httr2: status code works")
test_that("httr2 status code works", {
  # httr2_obj <- request(hb("/getttttt"))
  # save(httr2_obj, file="tests/testthat/httr2_obj.rda", version = 2L)
  load("httr2_obj.rda")

  expect_is(httr2_obj, "httr2_request")

  x <- RequestHandlerHttr2$new(httr2_obj)

  expect_is(x, "RequestHandlerHttr2")
  expect_is(x$handle, "function")
  expect_error(x$handle())

  # do request
  insert_cassette("bluecow")
  the_response <- x$handle()
  # the_response <- httr2::last_response()

  expect_is(the_response, "httr2_response")
  # status code is correct
  expect_equal(the_response$status_code, 404)

  eject_cassette("bluecow")

  # call again
  insert_cassette("bluecow")
  x$handle()
  response2 <- httr2::last_response()

  expect_is(response2, "httr2_response")
  # status code is correct
  expect_equal(response2$status_code, 404)

  eject_cassette("bluecow")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "bluecow"))
})


context("adapter-httr2: use_cassette works")
test_that("httr2 use_cassette works", {
  out <- use_cassette("httr2_test1", {
    x <- request(hb("/get")) %>% req_perform()
  })
  invisible(use_cassette("httr2_test1", {
    x2 <- request(hb("/get")) %>% req_perform()
  }))

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr2_test1")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # request - 1st http call
  expect_is(x$request, "httr2_request")
  expect_equal(x$request$method, "GET")
  expect_equal(x$request$url, hb("/get"))
  expect_named(x$request$headers, NULL)
  expect_is(x$request$fields, "list")
  
  # request - 2nd http call
  expect_is(x2$request, "httr2_request")
  expect_equal(x2$request$method, "GET")
  expect_equal(x2$request$url, hb("/get"))
  expect_named(x2$request$headers, NULL)
  expect_null(x2$request$fields)

  # response - stuff
  expect_is(x, "httr2_response")
  expect_equal(x$status_code, 200)
  expect_equal(x$url, hb("/get"))

  # fixture file
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  expect_is(str[[1]]$response$body$string, "character")
  expect_match(str[[1]]$response$body$string, "headers")
  expect_match(str[[1]]$response$body$string, "libcurl")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr2_test1.yml"))
})


context("adapter-httr2: use_cassette w/ preserve_exact_body_bytes")
test_that("httr2 use_cassette works", {
  out <- use_cassette("httr2_test2", {
    x <- request(hb("/get")) %>% req_perform()
  }, preserve_exact_body_bytes = TRUE)

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr2_test2")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # response
  expect_is(x, "httr2_response")
  expect_equal(x$status_code, 200)
  expect_equal(x$url, hb("/get"))

  # response body
  str <- yaml::yaml.load_file(out$manfile)
  str <- rawToChar(base64enc::base64decode(
    str$http_interactions[[1]]$response$body$base64_string))
  expect_is(str, "character")
  expect_match(str, "Connection")
  expect_match(str, "httpbin")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr2_test2.yml"))
})

context("adapter-httr2: use_cassette w/ req_error")
test_that("httr2 w/ req_error", {
  out <- use_cassette("httr2_errors_modify_with_req_error", {
    x404 <- request(hb("/status/404")) %>%
      req_error(is_error = function(resp) FALSE) %>% 
      req_perform()
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("httr2_errors_modify_with_req_error", {
    x404 <- request(hb("/status/404")) %>%
      req_error(is_error = function(resp) FALSE) %>% 
      req_perform()
  })

  expect_equal(x404$status_code, 404)

  # cassette
  expect_is(out, "Cassette")
  expect_match(out$manfile, "httr2_errors_modify_with_req_error")
  expect_false(out$is_empty())
  expect_is(out$recorded_at, "POSIXct")

  # response
  expect_is(x404, "httr2_response")
  expect_equal(x404$status_code, 404)

  # response body
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  expect_is(str, "list")
  expect_is(str[[1]], "list")
  expect_match(str[[1]]$request$uri , "404")

  # cleanup
  unlink(file.path(vcr_configuration()$dir,
    "httr2_errors_modify_with_req_error.yml"))
})

context("adapter-httr2: use_cassette just catch error")
test_that("httr2 error", {
  use_cassette("httr2_errors_catch_error", {
    expect_error(
      request(hb("/status/404")) %>% req_perform()
    )
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("httr2_errors_catch_error", {
    expect_error(
      request(hb("/status/404")) %>% req_perform()
    )
  })

  # cleanup
  unlink(file.path(vcr_configuration()$dir,
    "httr2_errors_catch_error.yml"))
})

context("adapter-httr2: use_cassette w/ multiple errors per cassette")
test_that("httr2 w/ multiple errors per cassette", {
  use_cassette("multiple_errors_per_cassette", {
    expect_error(request(hb("/status/404")) %>% req_perform())
    expect_error(request(hb("/status/500")) %>% req_perform())
    expect_error(request(hb("/status/418")) %>% req_perform())
  })
  # let's do it again to make sure using a cassette w/ errors still works
  use_cassette("multiple_errors_per_cassette", {
    expect_error(request(hb("/status/404")) %>% req_perform())
    expect_error(request(hb("/status/500")) %>% req_perform())
    expect_error(request(hb("/status/418")) %>% req_perform())
  })

  # cleanup
  unlink(file.path(vcr_configuration()$dir,
    "multiple_errors_per_cassette.yml"))
})

## httr removes the header, but with httr2 we have to explicity remove it
context("adapter-httr2: use_cassette w/ simple auth")
test_that("httr2 works with simple auth and hides auth details", {
  # Authorization header IS in the cassette after filtering
  use_cassette("httr2_test_simple_auth_no_filter", {
    x <- request(hb("/basic-auth/foo/bar")) %>%
      req_auth_basic("foo", "bar") %>%
      req_perform()
  })

  path <- file.path(vcr_configuration()$dir, "httr2_test_simple_auth_no_filter.yml")
  chars <- paste0(readLines(path), collapse = "")
  yml <- yaml::yaml.load_file(path)

  expect_true(grepl("Authorization", chars))
  expect_true("Authorization" %in% names(yml$http_interactions[[1]]$request$headers))

  # Authorization header IS NOT in the cassette after filtering
  vcr_configure(dir = tempdir(), filter_request_headers = "Authorization")
  use_cassette("httr2_test_simple_auth_yes_filter", {
    x <- request(hb("/basic-auth/foo/bar")) %>%
      req_auth_basic("foo", "bar") %>%
      req_perform()
  })

  path <- file.path(vcr_configuration()$dir, "httr2_test_simple_auth_yes_filter.yml")
  chars <- paste0(readLines(path), collapse = "")
  yml <- yaml::yaml.load_file(path)

  expect_false(grepl("Authorization", chars))
  expect_false("Authorization" %in% names(yml$http_interactions[[1]]$request$headers))

  # back to default vcr config
  vcr_configure(dir = tempdir())
  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr2_test_simple_auth_no_filter.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr2_test_simple_auth_yes_filter.yml"))
})

context("adapter-httr2: POST requests works")
test_that("httr2 POST requests works", {
  # body type: named list
  out <- use_cassette("httr2_post_named_list", {
    x <- request(hb("/post")) %>%
      req_body_json(list(foo = "bar")) %>%
      req_perform()
  })
  expect_false(out$is_empty())
  expect_is(x, "httr2_response")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "{\"foo\":\"bar\"}")

  # body type: character
  out2 <- use_cassette("httr2_post_string", {
    z <- request(hb("/post")) %>%
      req_body_raw("some string") %>%
      req_perform()
  })
  expect_false(out2$is_empty())
  expect_is(z, "httr2_response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # body type: raw
  out3 <- use_cassette("httr2_post_raw", {
    z <- request(hb("/post")) %>%
      req_body_raw(charToRaw("some string")) %>%
      req_perform()
    # z <- POST(hb("/post"), body = charToRaw("some string"))
  })
  expect_false(out3$is_empty())
  expect_is(z, "httr2_response")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "some string")

  # file with req_body_file
  ff <- tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette("httr2_post_body_file", {
    b <- request(hb("/post")) %>%
      req_body_file(ff) %>%
      req_perform()
  })
  expect_false(out4$is_empty())
  expect_is(b, "httr2_response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(length(strj$files$y), 0) # files empty
  expect_match(strj$data, "hello world") # data not empty
  unlink(ff)

  # using multipart
  gg <- tempfile(fileext = ".txt")
  cat("hello world\n", file = gg)
  out4 <- use_cassette("httr2_post_body_multipart", {
    b <- request(hb("/post")) %>%
      req_body_multipart(a = curl::form_file(gg), b = "some data") %>% 
      req_perform()
  })
  expect_false(out4$is_empty())
  expect_is(b, "httr2_response")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$a, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty
  unlink(gg)

  # body type: NULL
  out5 <- use_cassette("httr2_post_null", {
    m <- request(hb("/post")) %>%
      req_body_raw("") %>%
      req_perform()
  })
  expect_false(out5$is_empty())
  expect_is(m, "httr2_response")
  expect_equal(m$status_code, 200)
  str <- yaml::yaml.load_file(out5$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "httr2_post_named_list.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr2_post_string.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr2_post_raw.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr2_post_upload_file.yml"))
  unlink(file.path(vcr_configuration()$dir, "httr2_post_null.yml"))
})
