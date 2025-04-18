test_that("filter_headers/request/remove", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

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
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

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
  skip_on_cran()

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
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  # response headers: remove only
  con <- crul::HttpClient$new(hb_remote("/get"))
  cas_nofilters <- use_cassette(name = "filterheaders_no_filtering", {
    res_nofilters <- con$get()
  })
  # Do filtering
  local_vcr_configure(filter_response_headers = c("date", "server"))
  cas1 <- use_cassette(name = "filterheaders_response_remove", {
    res1 <- con$get()
  })
  cas2 <- use_cassette(name = "filterheaders_response_remove", {
    res2 <- con$get()
  })

  # with no filtering, response headers have date and server
  expect_true(all(
    c("date", "server") %in% names(res_nofilters$response_headers)
  ))
  # with filtering, 1st request, response headers have date and server
  expect_true(all(c("date", "server") %in% names(res1$response_headers)))
  # with filtering, subsequent requests, response headers DO NOT have date and server
  expect_false(all(c("date", "server") %in% names(res2$response_headers)))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # date and server found in cassette w/o filtering
  expect_true(all(
    c("date", "server") %in%
      names(yaml_no_filter$http_interactions[[1]]$response$headers)
  ))
  # date and server NOT found in cassette w/ filtering
  expect_false(all(
    c("date", "server") %in%
      names(yaml1$http_interactions[[1]]$response$headers)
  ))
  # casette objects from both requests identical
  expect_identical(
    yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file())
  )
})

test_that("filter_headers/response/replace", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  # response headers: replace only
  con <- crul::HttpClient$new(hb_remote("/get"))
  cas_nofilters <- use_cassette(name = "filterheaders_no_filtering", {
    res_nofilters <- con$get()
  })
  # Do filtering
  local_vcr_configure(
    filter_response_headers = list(server = "who-dis!?")
  )
  cas1 <- use_cassette(name = "filterheaders_response_replace", {
    res1 <- con$get()
  })
  cas2 <- use_cassette(name = "filterheaders_response_replace", {
    res2 <- con$get()
  })

  # with no filtering, response headers have date and server
  expect_false("who-dis!?" == res_nofilters$response_headers$server)
  # with filtering, 1st request, response headers have date and server
  expect_false("who-dis!?" == res1$response_headers$server)
  # with filtering, subsequent requests, response headers DO NOT have date and server
  expect_true("who-dis!?" == res2$response_headers$server)

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # date and server found in cassette w/o filtering
  expect_false(
    "who-dis!?" == yaml_no_filter$http_interactions[[1]]$response$headers$server
  )
  # date and server NOT found in cassette w/ filtering
  expect_true(
    "who-dis!?" == yaml1$http_interactions[[1]]$response$headers$server
  )
  # casette objects from both requests identical
  expect_identical(
    yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file())
  )
})

#### JSON - just checking 1 of the above with json
test_that("filter_headers/request/remove/json", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir(), serialize_with = 'json')

  # request headers: remove only
  # no header filtering to compare below stuff to
  con <- crul::HttpClient$new(hb("/get"), headers = list(Foo = "bar"))
  cas_nofilters <- use_cassette(name = "filterheaders_no_filtering", {
    res_nofilters <- con$get()
  })
  # Do filtering
  local_vcr_configure(filter_request_headers = c("Foo", "Accept"))
  con <- crul::HttpClient$new(hb("/get"), headers = list(Foo = "bar"))
  cas1 <- use_cassette(name = "filterheaders_remove_json", {
    res1 <- con$get()
  })
  cas2 <- use_cassette(name = "filterheaders_remove_json", {
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
  json1 <- jsonlite::fromJSON(cas1$file(), FALSE)
  json_no_filter <- jsonlite::fromJSON(cas_nofilters$file(), FALSE)
  # Foo found in cassette w/o filtering
  expect_true(
    "Foo" %in% names(json_no_filter$http_interactions[[1]]$request$headers)
  )
  # User-Agent in cassette
  expect_true(
    "User-Agent" %in% names(json1$http_interactions[[1]]$request$headers)
  )
  # Accept in no filtered cassette
  expect_false(
    "Accept" %in% names(json1$http_interactions[[1]]$request$headers)
  )
  # Accept not in cassette w/o filters
  expect_true(
    "Accept" %in% names(json_no_filter$http_interactions[[1]]$request$headers)
  )
  # Accept-Encoding in both, not filtered in either
  expect_true(
    "Accept-Encoding" %in% names(json1$http_interactions[[1]]$request$headers)
  )
  expect_true(
    "Accept-Encoding" %in%
      names(json_no_filter$http_interactions[[1]]$request$headers)
  )
  # casette objects from both requests identical
  expect_identical(
    jsonlite::fromJSON(cas1$file(), FALSE),
    jsonlite::fromJSON(cas2$file(), FALSE)
  )
})
