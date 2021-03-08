vcr_configure_reset()

test_that("filter_query_parameters: fails well", {
  expect_error(vcr_configure(filter_query_parameters = list(a = 1:3)))
})

vcr_configure_reset()

test_that("filter_query_parameters: remove", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "filter_query_parameters")

  # remove only
  # no query param filtering to compare below stuff to
  vcr_configure(dir = mydir)
  con <- crul::HttpClient$new("https://eu.httpbin.org/get")
  unlink(file.path(vcr_c$dir, "filterparams_no_filtering.yml"))
  cas_nofilters <- use_cassette(name = "filterparams_no_filtering", {
    res_nofilters <- con$get(query = list(Foo="bar"))
  })
  # Do filtering
  vcr_configure_reset()
  vcr_configure(dir = mydir, filter_query_parameters = "Foo")
  unlink(file.path(vcr_c$dir, "filterparams_remove.yml"))
  con <- crul::HttpClient$new("https://eu.httpbin.org/get")
  cas1 <- use_cassette(name = "filterparams_remove", {
    res1 <- con$get(query = list(Foo="bar"))
  })
  cas2 <- use_cassette(name = "filterparams_remove", {
    res2 <- con$get(query = list(Foo="bar"))
  })
  
  # with no filtering, request headers have Foo
  expect_true(grepl("Foo", res_nofilters$request$url$url))
  # with filtering, request params still in real request
  expect_true(grepl("Foo", res1$request$url$url))
  # again, on subsequent request, Foo is still there
  expect_true(grepl("Foo", res2$request$url$url))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(grepl("Foo", yaml_no_filter$http_interactions[[1]]$request$uri))
  # Foo NOT found in cassette w/ filtering
  expect_false(grepl("Foo", yaml1$http_interactions[[1]]$request$uri))
  # casette objects from both requests identical
  expect_identical(yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file()))
})

vcr_configure_reset()

test_that("filter_query_parameters: replace", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "filter_query_parameters")

  # remove only
  # no query param filtering to compare below stuff to
  vcr_configure(dir = mydir)
  con <- crul::HttpClient$new("https://eu.httpbin.org/get")
  unlink(file.path(vcr_c$dir, "filterparams_no_filtering.yml"))
  cas_nofilters <- use_cassette(name = "filterparams_no_filtering", {
    res_nofilters <- con$get(query = list(Foo="bar"))
  })
  # Do filtering
  vcr_configure_reset()
  vcr_configure(dir = mydir, filter_query_parameters = list(Foo = "placeholder"))
  unlink(file.path(vcr_c$dir, "filterparams_replace.yml"))
  con <- crul::HttpClient$new("https://eu.httpbin.org/get")
  cas1 <- use_cassette(name = "filterparams_replace", {
    res1 <- con$get(query = list(Foo="bar"))
  })
  cas2 <- use_cassette(name = "filterparams_replace", {
    res2 <- con$get(query = list(Foo="bar"))
  })
  
  # with no filtering, request headers have Foo
  expect_true(grepl("Foo", res_nofilters$request$url$url))
  # with filtering, request params still in real request
  expect_true(grepl("Foo", res1$request$url$url))
  # again, on subsequent request, Foo is still there
  expect_true(grepl("Foo", res2$request$url$url))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(grepl("bar", yaml_no_filter$http_interactions[[1]]$request$uri))
  # Foo NOT found in cassette w/ filtering
  expect_false(grepl("bar", yaml1$http_interactions[[1]]$request$uri))
  # casette objects from both requests identical
  expect_identical(yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file()))
})

vcr_configure_reset()

test_that("filter_query_parameters: replace with secret", {
  skip_on_cran()

  library(crul)
  con <- crul::HttpClient$new("https://eu.httpbin.org/get")
  mydir <- file.path(tempdir(), "filter_query_parameters_replace_with")
  Sys.setenv(MY_KEY = "my-secret-key")
  # Sys.getenv("MY_KEY")

  # remove only
  # no query param filtering to compare below stuff to
  vcr_configure(dir = mydir)
  unlink(file.path(vcr_c$dir, "filterparams_no_filtering.yml"))
  cas_nofilters <- use_cassette(name = "filterparams_no_filtering", {
    res_nofilters <- con$get(query = list(Foo=Sys.getenv("MY_KEY")))
  })
  # Do filtering
  vcr_configure_reset()
  vcr_configure(dir = mydir, filter_query_parameters = list(Foo = c(Sys.getenv("MY_KEY"), "bar")))
  unlink(file.path(vcr_c$dir, "filterparams_replacewith.yml"))
  cas1 <- use_cassette(name = "filterparams_replacewith", {
    res1 <- con$get(query = list(Foo=Sys.getenv("MY_KEY")))
  })
  cas2 <- use_cassette(name = "filterparams_replacewith", {
    res2 <- con$get(query = list(Foo=Sys.getenv("MY_KEY")))
  })
  
  # with no filtering, request headers have Foo
  expect_true(grepl("my-secret-key", res_nofilters$request$url$url))
  # with filtering, request params still in real request
  expect_true(grepl("my-secret-key", res1$request$url$url))
  # again, on subsequent request, Foo is still there
  expect_true(grepl("my-secret-key", res2$request$url$url))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(grepl("my-secret-key", yaml_no_filter$http_interactions[[1]]$request$uri))
  # Foo NOT found in cassette w/ filtering
  expect_false(grepl("my-secret-key", yaml1$http_interactions[[1]]$request$uri))
  # casette objects from both requests identical
  expect_identical(yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file()))

  Sys.unsetenv("MY_KEY")
})

vcr_configure_reset()

test_that("filter_query_parameters: remove (httr)", {
  skip_on_cran()

  library(httr)
  mydir <- file.path(tempdir(), "filter_query_parameters_httr")

  # remove only
  # no query param filtering to compare below stuff to
  vcr_configure(dir = mydir)
  unlink(file.path(vcr_c$dir, "filterparams_no_filtering.yml"))
  cas_nofilters <- use_cassette(name = "filterparams_no_filtering", {
    res_nofilters <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  # Do filtering
  vcr_configure_reset()
  vcr_configure(dir = mydir, filter_query_parameters = "Foo")
  unlink(file.path(vcr_c$dir, "filterparams_remove.yml"))
  cas1 <- use_cassette(name = "filterparams_remove", {
    res1 <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  cas2 <- use_cassette(name = "filterparams_remove", {
    res2 <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  
  # with no filtering, request headers have Foo
  expect_true(grepl("Foo", res_nofilters$request$url))
  # with filtering, request params still in real request
  expect_true(grepl("Foo", res1$request$url))
  # again, on subsequent request, Foo is still there
  expect_true(grepl("Foo", res2$request$url))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(grepl("Foo", yaml_no_filter$http_interactions[[1]]$request$uri))
  # Foo NOT found in cassette w/ filtering
  expect_false(grepl("Foo", yaml1$http_interactions[[1]]$request$uri))
  # casette objects from both requests identical
  expect_identical(yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file()))
})

vcr_configure_reset()

test_that("filter_query_parameters: replace (httr)", {
  skip_on_cran()

  library(httr)
  mydir <- file.path(tempdir(), "filter_query_parameters_httr")

  # remove only
  # no query param filtering to compare below stuff to
  vcr_configure(dir = mydir)
  unlink(file.path(vcr_c$dir, "filterparams_no_filtering.yml"))
  cas_nofilters <- use_cassette(name = "filterparams_no_filtering", {
    res_nofilters <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  # Do filtering
  vcr_configure_reset()
  vcr_configure(dir = mydir, filter_query_parameters = list(Foo = "placeholder"))
  unlink(file.path(vcr_c$dir, "filterparams_replace.yml"))
  cas1 <- use_cassette(name = "filterparams_replace", {
    res1 <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  cas2 <- use_cassette(name = "filterparams_replace", {
    res2 <- GET("https://eu.httpbin.org/get?Foo=bar")
  })
  
  # with no filtering, request headers have Foo
  expect_true(grepl("Foo", res_nofilters$request$url))
  # with filtering, request params still in real request
  expect_true(grepl("Foo", res1$request$url))
  # again, on subsequent request, Foo is still there
  expect_true(grepl("Foo", res2$request$url))

  # compare cassettes
  yaml1 <- yaml::yaml.load_file(cas1$file())
  yaml_no_filter <- yaml::yaml.load_file(cas_nofilters$file())
  # Foo found in cassette w/o filtering
  expect_true(grepl("bar", yaml_no_filter$http_interactions[[1]]$request$uri))
  # Foo NOT found in cassette w/ filtering
  expect_false(grepl("bar", yaml1$http_interactions[[1]]$request$uri))
  # casette objects from both requests identical
  expect_identical(yaml::yaml.load_file(cas1$file()),
    yaml::yaml.load_file(cas2$file()))
})

vcr_configure_reset()
