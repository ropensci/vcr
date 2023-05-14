test_that("use_cassette: match on body w/ empty body", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  cli <- HttpClient$new(url = hb())

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(name = "testing9", {
    res <- cli$post("post")
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing9", {
    res <- cli$post("post")
  }, match_requests_on = c("method", "uri", "body"))
    
  # the request body in the crul request is empty
  expect_null(res$request$body)

  # the request body in the cassette is an empty string
  cas <- yaml::yaml.load_file(file.path(mydir, "testing9.yml"))
  expect_equal(cas$http_interactions[[1]]$request$body$string, "")

  # NOTE: internally, the NULL in the request body gets turned into 
  # an empty string, so we end up comparing an empty string to an empty
  # string (in cases where no request bodies are sent or in a cassette
  # that is)
  
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing9")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  # cleanup
  unlink(mydir, recursive = TRUE)
})

# cleanup
# reset configuration
vcr_configure_reset()
