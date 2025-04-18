test_that("RequestIgnorer basic stuff", {
  aa <- RequestIgnorer$new()
  expect_s3_class(aa, "R6")
  expect_s3_class(aa, "RequestIgnorer")

  # vars
  expect_equal(aa$LOCALHOST_ALIASES, c('localhost', '127.0.0.1', '0.0.0.0'))
  expect_s3_class(aa$ignored_hosts, "EnvHash")

  # methods
  expect_type(aa$ignore_localhost, "closure")
  expect_type(aa$ignore_localhost_value, "closure")
  expect_type(aa$ignore_hosts, "closure")
  expect_type(aa$should_be_ignored, "closure")
})

test_that("RequestIgnorer usage", {
  x <- RequestIgnorer$new()
  x$ignore_hosts(hosts = "google.com")
  expect_equal(x$ignored_hosts$bucket, "google.com")
  x$ignore_localhost()
  expect_equal(x$ignored_hosts$bucket, c("google.com", x$LOCALHOST_ALIASES))

  x <- RequestIgnorer$new()
  x$ignore_localhost_value('127.0.0.1')
  expect_equal(x$ignored_hosts$bucket, '127.0.0.1')

  x <- RequestIgnorer$new()
  expect_false(x$should_be_ignored(list(uri = "google.com")))
  x$ignore_hosts(hosts = "google.com")
  expect_true(x$should_be_ignored(list(uri = "google.com")))
  expect_false(x$should_be_ignored(list(uri = "foo.com")))
})

test_that("RequestIgnorer usage: w/ real requests", {
  skip_on_cran()

  # ignore by host
  data(crul_request)
  crul_request$url$handle <- curl::new_handle()
  z <- RequestHandlerCrul$new(crul_request)
  expect_error(z$handle())
  the$request_ignorer$ignore_hosts(hosts = "api.crossref.org")
  z$handle()

  # ignore localhost
  req <- list(
    url = list(url = "http://127.0.0.1"),
    headers = NULL,
    method = "get",
    options = list(httpget = TRUE, useragent = "crul/0.5.4")
  )
  req$url$handle <- curl::new_handle()
  z <- RequestHandlerCrul$new(req)
  expect_error(z$handle(), "vcr does not know how to handle")
  the$request_ignorer$ignore_localhost()
  if (Sys.info()[['sysname']] != "Windows") {
    expect_error(z$handle(), "Failed to connect")
  }

  # cleanup
  rm(crul_request)

  # ignore by callback - FIXME: we're not yet supporting a user defined fxn
  # req <- list(url = list(url = "http://127.0.0.1"),
  #   method = "get", options = list(httpget = TRUE, useragent = "crul/0.5.4"))
  # req$url$handle <- curl::new_handle()
  # z <- RequestHandlerCrul$new(req)
  # expect_error(z$handle(), "vcr does not know how to handle")
  # x <- RequestIgnorer$new()
  # x$ignore_localhost()
  # expect_error(z$handle(), "Failed to connect")
})

test_that("RequestIgnorer usage: w/ vcr_configure() usage", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  # Ingore nothing
  use_cassette("test-1", {
    crul::HttpClient$new("https://google.com")$get()
    crul::HttpClient$new("https://scottchamberlain.info")$get()
  })
  cas <- read_cassette("test-1.yml")
  expect_equal(length(cas$http_interactions), 2)

  # Ignore host
  local_vcr_configure(ignore_hosts = "google.com")
  use_cassette("test-2", {
    crul::HttpClient$new("https://google.com")$get()
    crul::HttpClient$new("https://scottchamberlain.info")$get()
  })
  cas <- read_cassette("test-2.yml")
  expect_equal(length(cas$http_interactions), 1)

  # Ignore localhost
  local_vcr_configure(ignore_localhost = TRUE)
  cas_local_ignored <- use_cassette("test-3", {
    crul::HttpClient$new("https://scottchamberlain.info")$get()
    crul::HttpClient$new(hb("/get"))$get()
  })
  cas <- read_cassette("test-3.yml")
  expect_equal(length(cas$http_interactions), 1)
})

test_that("RequestIgnorer fails well", {
  expect_error(RequestIgnorer$new(a = 5), "unused argument")
  z <- RequestIgnorer$new()
  expect_error(z$ignore_hosts(), "missing")
  expect_error(z$ignore_localhost_value(), "missing")
})
