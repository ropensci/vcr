context("serializers: JSON")

test_that("JSON basic stuff", {
  expect_is(JSON, "R6ClassGenerator")
  aa <- JSON$new()
  expect_is(aa, "R6")
  expect_is(aa, "JSON")

  # vars
  expect_is(aa$file_extension, "character")
  expect_equal(aa$file_extension, ".json")
  expect_is(aa$path, "character")
  expect_match(aa$path, "\\.json")
  expect_null(aa$string)

  # methods
  expect_is(aa$serialize, "function")
  expect_is(aa$deserialize, "function")
})

test_that("JSON usage", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir, serialize_with = 'json'))
  unlink(file.path(vcr_c$dir, "testing1.json"))
  vcr_configuration()

  # does one request work?
  aa <- use_cassette("testing2", {
    res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  })
  expect_is(aa, "Cassette")
  expect_is(res, "HttpResponse")
  expect_length(jsonlite::fromJSON(aa$file(), FALSE)[[1]], 1)

  # do two requests work?
  cc <- use_cassette("testing4", {
    ref <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
    the <- crul::HttpClient$new("https://eu.httpbin.org/post")$post(body = "fafaa")
  })
  expect_is(ref, "HttpResponse")
  expect_is(the, "HttpResponse")
  expect_length(jsonlite::fromJSON(cc$file(), FALSE)[[1]], 2)

  # preserve exact body bytes
  dd <- use_cassette("testing5", {
    raf <- crul::HttpClient$new("https://eu.httpbin.org/get")$get(
      query=list(cheese="string"))
    raz <- crul::HttpClient$new(
      "https://eu.httpbin.org/post"
      )$post(body = list(foo = "bar", baz = "ball"))
  }, preserve_exact_body_bytes = TRUE)
  expect_is(raf, "HttpResponse")
  expect_is(raz, "HttpResponse")
  expect_length(jsonlite::fromJSON(dd$file(), FALSE)[[1]], 2)
  bodies <- jsonlite::fromJSON(dd$file())[[1]]$response$body$string
  for (i in bodies) expect_true(is_base64(i))
})

test_that("JSON fails well", {
  expect_error(JSON$new(a = 5), "unused argument")

  z <- JSON$new()
  # if no path specified, fails with useful message as is
  expect_error(suppressWarnings(z$deserialize()),
    "cannot open the connection")
})
