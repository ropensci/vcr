context("RequestIgnorer")

test_that("RequestIgnorer basic stuff", {
  expect_is(RequestIgnorer, "R6ClassGenerator")
  aa <- RequestIgnorer$new()
  expect_is(aa, "R6")
  expect_is(aa, "RequestIgnorer")

  # vars
  expect_equal(aa$LOCALHOST_ALIASES, c('localhost', '127.0.0.1', '0.0.0.0'))
  expect_is(aa$ignored_hosts, "EnvHash")

  # methods
  expect_is(aa$ignore_localhost, "function")
  expect_is(aa$ignore_localhost_value, "function")
  expect_is(aa$ignore_request, "function")
  expect_is(aa$ignore_hosts, "function")
  expect_is(aa$should_be_ignored, "function")
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
  expect_false(VCRHooks$hooks$ignore_request(list(uri = "google.com")))
  x$ignore_hosts(hosts = "google.com")
  expect_true(VCRHooks$hooks$ignore_request(list(uri = "google.com")))
  expect_false(VCRHooks$hooks$ignore_request(list(uri = "foo.com")))
})

test_that("RequestIgnorer usage: w/ real requests", {
  skip_on_cran()

  # ignore by host
  data(crul_request)
  crul_request$url$handle <- curl::new_handle()
  z <- RequestHandlerCrul$new(crul_request)
  expect_error(z$handle())
  x <- RequestIgnorer$new()
  x$ignore_hosts(hosts = "api.crossref.org")
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
  x <- RequestIgnorer$new()
  x$ignore_localhost()
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

  on.exit({
    files <- c(
      "test_ignore_host.yml",
      "test_ignore_host_ignored.yml",
      "test_ignore_localhost_ignored.yml",
      "test_ignore_localhost.yml")
    unlink(file.path(vcr_configuration()$dir, files))
  })

  library(crul)

  vcr_configure_reset()

  # IGNORE BY HOST
  tmpdir <- tempdir()
  vcr_configure(dir = tmpdir)
  # vcr_configuration()
  cas_not_ignored <- use_cassette("test_ignore_host", {
    HttpClient$new(hb())$get()
    HttpClient$new("https://scottchamberlain.info")$get()
  })

  vcr_configure_reset()

  vcr_configure(dir = tmpdir, ignore_hosts = "google.com")
  # vcr_configuration()
  cas_ignored <- use_cassette("test_ignore_host_ignored", {
    HttpClient$new("https://google.com")$get()
    HttpClient$new("https://scottchamberlain.info")$get()
  })

  read_cas <- function(x) suppressWarnings(yaml::yaml.load_file(x))$http_interactions
  expect_equal(length(read_cas(cas_not_ignored$file())), 2)
  expect_equal(length(read_cas(cas_ignored$file())), 1)


  # IGNORE LOCALHOST
  # Start python simple server on cli: python3 -m http.server
  skip_if_localhost_8000_gone()
  tmpdir <- tempdir()
  vcr_configure(dir = tmpdir)
  cas_local_not_ignored <- use_cassette("test_ignore_localhost", {
    HttpClient$new("https://scottchamberlain.info")$get()
    HttpClient$new("http://127.0.0.1:8000")$get()
  })

  vcr_configure(dir = tmpdir, ignore_localhost = TRUE)
  # vcr_configuration()
  cas_local_ignored <- use_cassette("test_ignore_localhost_ignored", {
    HttpClient$new("https://scottchamberlain.info")$get()
    HttpClient$new("http://127.0.0.1:8000")$get()
  })

  expect_equal(length(read_cas(cas_local_not_ignored$file())), 2)
  expect_equal(length(read_cas(cas_local_ignored$file())), 1)
})

# reset
vcr_configure(dir = tmpdir)

test_that("RequestIgnorer fails well", {
  expect_error(RequestIgnorer$new(a = 5), "unused argument")
  z <- RequestIgnorer$new()
  expect_error(z$ignore_hosts(), "missing")
  expect_error(z$ignore_localhost_value(), "missing")
  expect_error(z$should_be_ignored(), "missing")
})
