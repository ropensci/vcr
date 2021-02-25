context("redirects")

test_that("redirects: w/ crul", {
  library(crul)
  mydir <- file.path(tempdir(), "bunnybear")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  con <- HttpClient$new(url = "https://hb.opencpu.org")

  # first recording
  cas1 <- use_cassette("testing2", {
    x1 <- con$get("redirect/3")
  }, record_separate_redirects = TRUE)

  # cassette
  expect_is(cas1, "Cassette")
  expect_true(cas1$record_separate_redirects)
  cas_file <- yaml.load_file(cas1$file())
  expect_length(cas_file$http_interactions, 4)
  locs <- unlist(lapply(cas_file$http_interactions, function(w) w$response$headers$location))
  expect_equal(locs, c("/relative-redirect/2", "/relative-redirect/1", "/get"))
  # response
  expect_is(x1, "HttpResponse")
  ## FIXME: this should be: expect_match(x1$url, "/get")
  expect_match(x1$url, "/redirect/3")

  # second recording
  cas2 <- use_cassette("testing2", {
    x2 <- con$get("redirect/3")
  }, record_separate_redirects = TRUE)

  # cassette
  expect_is(cas2, "Cassette")
  expect_true(cas2$record_separate_redirects)
  cas2_file <- yaml.load_file(cas2$file())
  expect_length(cas2_file$http_interactions, 4)
  locs2 <- unlist(lapply(cas2_file$http_interactions, function(w) w$response$headers$location))
  expect_equal(locs2, c("/relative-redirect/2", "/relative-redirect/1", "/get"))
  # response
  expect_is(x2, "HttpResponse")
  ## FIXME: this should be: expect_match(x1$url, "/get")
  expect_match(x2$url, "/redirect/3")

  # cleanup
  unlink(mydir, recursive = TRUE)
})

test_that("redirects: w/ httr", {
  library(httr)
  mydir <- file.path(tempdir(), "bunnybear2")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))

  # first recording
  cas1 <- use_cassette("testing3", {
    x1 <- GET("https://hb.opencpu.org/redirect/3")
  }, record_separate_redirects = TRUE)

  # cassette
  expect_is(cas1, "Cassette")
  expect_true(cas1$record_separate_redirects)
  cas_file <- yaml.load_file(cas1$file())
  expect_length(cas_file$http_interactions, 4)
  locs <- unlist(lapply(cas_file$http_interactions, function(w) w$response$headers$location))
  expect_equal(locs, c("/relative-redirect/2", "/relative-redirect/1", "/get"))
  # response
  expect_is(x1, "response")
  ## FIXME: this should be: expect_match(x1$url, "/get")
  expect_match(x1$url, "/redirect/3")

  # second recording
  cas2 <- use_cassette("testing4", {
    x2 <- GET("https://hb.opencpu.org/redirect/3")
  }, record_separate_redirects = TRUE)

  # cassette
  expect_is(cas2, "Cassette")
  expect_true(cas2$record_separate_redirects)
  cas2_file <- yaml.load_file(cas2$file())
  expect_length(cas2_file$http_interactions, 4)
  locs2 <- unlist(lapply(cas2_file$http_interactions, function(w) w$response$headers$location))
  expect_equal(locs2, c("/relative-redirect/2", "/relative-redirect/1", "/get"))
  # response
  expect_is(x2, "response")
  ## FIXME: this should be: expect_match(x1$url, "/get")
  expect_match(x2$url, "/redirect/3")

  # cleanup
  unlink(mydir, recursive = TRUE)
})

# cleanup
# reset configuration
vcr_configure_reset()
