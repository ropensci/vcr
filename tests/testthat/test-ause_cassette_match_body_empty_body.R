test_that("use_cassette: match on body w/ empty body", {
  local_vcr_configure(dir = withr::local_tempdir())
  cli <- crul::HttpClient$new(url = hb())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(
    "testing9",
    {
      res <- cli$post("post")
    },
    match_requests_on = c("method", "uri", "body")
  )
  aa <- use_cassette(
    "testing9",
    {
      res <- cli$post("post")
    },
    match_requests_on = c("method", "uri", "body")
  )
  # run it again
  bb <- use_cassette(
    "testing9",
    {
      res <- cli$post("post")
    },
    match_requests_on = c("method", "uri", "body")
  )

  # the request body in the crul request is empty
  expect_null(res$request$body)

  # the request body in the cassette is an empty string
  cas <- read_cassette("testing9.yml")
  expect_equal(cas$http_interactions[[1]]$request$body$string, NULL)

  # NOTE: internally, the NULL in the request body gets turned into
  # an empty string, so we end up comparing an empty string to an empty
  # string (in cases where no request bodies are sent or in a cassette
  # that is)

  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing9")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))
})
