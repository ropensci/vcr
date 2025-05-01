test_that("by default, headers left unchanged", {
  headers <- list(x = "a", y = "b")
  expect_equal(encode_headers(headers, "request"), headers)
  expect_equal(encode_headers(headers, "response"), headers)

  # even empty headers
  headers <- list()
  expect_equal(encode_headers(headers, "request"), headers)
  expect_equal(encode_headers(headers, "response"), headers)
})

test_that("filter_headers/request/remove", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("uri", "headers")
  )

  # request headers: remove only
  # no header filtering to compare below stuff to
  con <- crul::HttpClient$new(hb("/get"), headers = list(Foo = "bar"))
  cas_nofilters <- use_cassette(name = "filterheaders_no_filtering", {
    res_nofilters <- con$get()
  })

  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_request_headers = c("Foo", "Accept")
  )
  con <- crul::HttpClient$new(hb("/get"), headers = list(Foo = "bar"))
  cas1 <- use_cassette(name = "filterheaders_remove", {
    res1 <- con$get()
  })
  cas2 <- use_cassette(name = "filterheaders_remove", {
    res2 <- con$get()
  })

  # with no filtering, request headers have Foo
  expect_true("Foo" %in% names(res_nofilters$request_headers))
  # with filtering, request headers clearly have Foo on first request
  expect_true("Foo" %in% names(res1$request_headers))
  # with filtering, request headers have Foo on subsequent requests b/c
  # header is being sent in request, so not filtered out of http
  # response object
  expect_true("Foo" %in% names(res2$request_headers))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(
    "Foo" %in% names(yaml_no_filter$http_interactions[[1]]$request$headers)
  )
  # User-Agent in cassette
  expect_true(
    "User-Agent" %in% names(yaml1$http_interactions[[1]]$request$headers)
  )
  # Accept in no filtered cassette
  expect_false(
    "Accept" %in% names(yaml1$http_interactions[[1]]$request$headers)
  )
  # Accept not in cassette w/o filters
  expect_true(
    "Accept" %in% names(yaml_no_filter$http_interactions[[1]]$request$headers)
  )
  # Accept-Encoding in both, not filtered in either
  expect_true(
    "Accept-Encoding" %in% names(yaml1$http_interactions[[1]]$request$headers)
  )
  expect_true(
    "Accept-Encoding" %in%
      names(yaml_no_filter$http_interactions[[1]]$request$headers)
  )
  # casette objects from both requests identical
  expect_identical(
    yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file())
  )
})

test_that("filter_headers/request/replace", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    match_requests_on = c("method", "uri", "headers")
  )

  # request headers: replace only
  # no header filtering to compare below stuff to
  con1 <- crul::HttpClient$new(
    hb("/get"),
    headers = list(Authorization = "mysecret")
  )
  cas_nofilters <- use_cassette(name = "filterheaders_no_filtering", {
    res_nofilters <- con1$get()
  })
  # Do filtering

  local_vcr_configure(
    filter_request_headers = list("Authorization" = "XXXXXXX")
  )
  cas_rep1 <- use_cassette(name = "filterheaders_replace", {
    res <- con1$get()
  })
  cas_rep2 <- use_cassette(name = "filterheaders_replace", {
    res2 <- con1$get()
  })

  # with or w/o filtering, request headers have Authorization="mysecret"
  invisible(lapply(list(res_nofilters, res, res2), function(z) {
    expect_equal(z$request_headers$Authorization, "mysecret")
  }))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas_rep1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # "mysecret" found in cassette W/O filtering
  expect_equal(
    yaml_no_filter$http_interactions[[1]]$request$headers$Authorization,
    "mysecret"
  )
  # "XXXXXXX" found in cassette WITH filtering
  expect_equal(
    yaml1$http_interactions[[1]]$request$headers$Authorization,
    "XXXXXXX"
  )
  # casette objects from both requests identical
  expect_identical(
    yaml::yaml.load_file(cas_rep1$file()),
    yaml::yaml.load_file(cas_rep2$file())
  )
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
