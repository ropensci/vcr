test_that("by default, headers left unchanged", {
  headers <- list(x = "a", y = "b")
  expect_equal(encode_headers(headers, "request"), headers)
  expect_equal(encode_headers(headers, "response"), headers)

  # even empty headers
  headers <- list()
  expect_equal(encode_headers(headers, "request"), headers)
  expect_equal(encode_headers(headers, "response"), headers)
})

test_that("can remove headers on disk", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("method", "uri", "headers"),
    filter_request_headers = "x"
  )

  con <- crul::HttpClient$new(hb("/get"), headers = list(x = "live"))
  use_cassette("test", res1 <- con$get())
  expect_equal(res1$request_headers$x, "live")

  interaction <- read_cassette("test.yml")$http_interactions[[1]]
  expect_equal(interaction$request$headers$x, NULL)

  use_cassette("test", res2 <- con$get())
  expect_equal(res2$request_headers$x, "live")
})

test_that("can replace headers on disk", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("method", "uri", "headers"),
    filter_request_headers = list(x = "ondisk")
  )

  con <- crul::HttpClient$new(hb("/get"), headers = list(x = "live"))
  use_cassette("test", res1 <- con$get())
  expect_equal(res1$request_headers$x, "live")

  interaction <- read_cassette("test.yml")$http_interactions[[1]]
  expect_equal(interaction$request$headers$x, "ondisk")

  use_cassette("test", res2 <- con$get())
  expect_equal(res2$request_headers$x, "live")
})

test_that("filter_headers doesn't add a header that doesn't exist", {
  mydir <- file.path(tempdir(), "filter_headers_doesnt_add_header")
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_request_headers = list("Authorization" = "XXXXXXX")
  )
  con1 <- crul::HttpClient$new(hb("/get"))
  cas_nh1 <- use_cassette(name = "filterheaders_no_header", {
    res <- con1$get()
  })
  cas_nh2 <- use_cassette(name = "filterheaders_no_header", {
    res2 <- con1$get()
  })

  # request's don't have Authorization header
  invisible(lapply(list(res, res2), function(z) {
    expect_null(z$request_headers$Authorization)
  }))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas_nh1$file())
  yaml2 <- yaml::yaml.load_file(cas_nh2$file())
  # no Authorization is added
  expect_null(yaml1$http_interactions[[1]]$request$headers$Authorization)
  # still no Authorization header in subsequent use_cassette runs
  expect_null(yaml2$http_interactions[[1]]$request$headers$Authorization)
})

test_that("filter_headers/response/remove", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_response_headers = "foo"
  )
  con <- crul::HttpClient$new(hb("/response-headers?foo=bar"))

  use_cassette("test", res1 <- con$get())
  # first response returns unfiltered headers
  expect_equal(res1$response_headers$foo, "bar")

  cassette <- read_cassette("test.yml")
  # but foo is removed on disk
  expect_equal(cassette$http_interactions[[1]]$response$headers$foo, NULL)

  # and in the recorded response
  use_cassette("test", res2 <- con$get())
  expect_equal(res2$response_headers$foo, NULL)
})

test_that("filter_headers/response/replace", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_response_headers = list(foo = "foo")
  )
  con <- crul::HttpClient$new(hb("/response-headers?foo=bar"))

  use_cassette("test", res1 <- con$get())
  # first response returns unfiltered headers
  expect_equal(res1$response_headers$foo, "bar")

  cassette <- read_cassette("test.yml")
  # but foo is recorded on disk
  expect_equal(cassette$http_interactions[[1]]$response$headers$foo, "foo")

  # and in the recorded response
  use_cassette("test", res2 <- con$get())
  expect_equal(res2$response_headers$foo, "foo")
})

test_that("dedup_keys", {
  # no modification
  x <- list(b = "foo", a = 5)
  expect_equal(dedup_keys(x), x)

  # modification: group the a keys
  x <- list(b = "foo", a = 5, a = 6)
  expect_equal(dedup_keys(x), list(a = c(5, 6), b = "foo"))

  # FIXME: doesn't yet work for nested duplicates. not sure if
  # we need it to work for this case or not?
  x <- list(b = "foo", c = list(a = 5, a = 6))
  expect_equal(dedup_keys(x), x)
})


test_that("Authorization is always redacted", {
  expect_equal(
    encode_headers(list(Authorization = "mysecret")),
    list(Authorization = "<redacted>")
  )

  # And it's case insensitive
  expect_equal(
    encode_headers(list(AUTHORIZATION = "mysecret")),
    list(AUTHORIZATION = "<redacted>")
  )
})
