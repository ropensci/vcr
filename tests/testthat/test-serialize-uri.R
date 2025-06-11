test_that("can remove query parameters", {
  expect_equal(
    encode_uri("https://example.com/?x=abc&y=def", "x"),
    "https://example.com/?y=def"
  )

  expect_equal(
    encode_uri("https://example.com/?x=abc&y=def", list("x", "y")),
    "https://example.com/"
  )
})

test_that("replace entire query parameters", {
  expect_equal(
    encode_uri("https://example.com/?x=abc&y=def", list(x = "XXX")),
    "https://example.com/?x=XXX&y=def"
  )
})

test_that("can replace part of query parameters", {
  expect_equal(
    encode_uri("https://example.com/?x=abc&y=def", list(x = c("a", "A"))),
    "https://example.com/?x=Abc&y=def"
  )
})

test_that("can do a combination of all of the above", {
  expect_equal(
    encode_uri(
      "https://example.com/?x=abc&y=def&z=ghi",
      list("x", y = "YYY", z = c("g", "G"))
    ),
    "https://example.com/?y=YYY&z=Ghi"
  )
})

test_that("filter_query_parameters: fails well", {
  expect_error(vcr_configure(filter_query_parameters = list(a = 1:3)))
})

test_that("query param is not recorded on disk", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_query_parameters = list("x", y = "Y")
  )
  con <- crul::HttpClient$new(hb("/get"))
  use_cassette("test", con$get(query = list(x = "x", y = "y")))

  expect_equal(vcr_last_request()$uri, hb("/get?y=Y"))
})
