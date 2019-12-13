test_that("use_cassette: match_requests_on - body works w/ crul", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  cli <- HttpClient$new(url = "https://httpbin.org")

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(name = "testing1", {
    res <- cli$post("post", body = list(foo = "bar"))
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing1", {
    res <- cli$post("post", body = list(foo = "bar"))
  }, match_requests_on = c("method", "uri", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  ### matchers: method, body (uri ignored essentially)
  # run it
  aa <- use_cassette(name = "testing2", {
    res <- cli$post("post", query = list(a = 5), body = list(foo = "bar"))
  }, match_requests_on = c("method", "body"))
  # run it again
  bb <- use_cassette(name = "testing2", {
    res <- cli$post("post", query = list(b = 2), body = list(foo = "bar"))
  }, match_requests_on = c("method", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "body"))

  ### matchers: body only
  # run it
  # FIXME: still not quite working 
  cli2 <- HttpClient$new(url = "https://stuff.com")
  aa <- use_cassette(name = "testing3", {
    res <- cli$put("put", body = list(foo = "bar"))
  }, match_requests_on = "body")
  # run it again, method and uri changed
  bb <- use_cassette(name = "testing3", {
    res2 <- cli$post("post", body = list(foo = "bar"))
  }, match_requests_on = "body")
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing3")
  expect_equal(aa$match_requests_on, "body")

  # cleanup
  unlink(mydir, recursive = TRUE)
})

test_that("use_cassette: match_requests_on - body works w/ httr", {
  skip_on_cran()

  library(httr)
  mydir <- file.path(tempdir(), "testing_httr")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))

  ### matchers: method, uri, body
  # run it
  aa <- use_cassette(name = "testing2", {
    res <- POST("https://httpbin.org/post", body = list(foo = "bar"))
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing2", {
    res <- POST("https://httpbin.org/post", body = list(foo = "bar"))
  }, match_requests_on = c("method", "uri", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing2")
  expect_equal(aa$match_requests_on, c("method", "uri", "body"))

  ### matchers: method, body (uri ignored essentially)
  # run it
  aa <- use_cassette(name = "testing4", {
    res <- POST("https://httpbin.org/post", query = list(a = 5), body = list(foo = "bar"))
  }, match_requests_on = c("method", "body"))
  # run it again
  bb <- use_cassette(name = "testing4", {
    res <- POST("https://httpbin.org/post", query = list(b = 2), body = list(foo = "bar"))
  }, match_requests_on = c("method", "body"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing4")
  expect_equal(aa$match_requests_on, c("method", "body"))

  ### matchers: body only
  # run it
  aa <- use_cassette(name = "testing5", {
    res <- POST("https://httpbin.org/post", body = list(foo = "bar"))
  }, match_requests_on = "body")
  # run it again, method and uri changed
  bb <- use_cassette(name = "testing5", {
    res <- PUT("https://httpbin.org/put", body = list(foo = "bar"))
  }, match_requests_on = "body")
  # run it again, method and uri changed again
  cc <- use_cassette(name = "testing5", {
    res <- PATCH("https://httpbin.org/patch", body = list(foo = "bar"))
  }, match_requests_on = "body")
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_identical(recorded_at(bb), recorded_at(cc))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing5")
  expect_equal(aa$match_requests_on, "body")
  expect_equal(bb$match_requests_on, "body")

  # cleanup
  unlink(mydir, recursive = TRUE)
})

# cleanup
# reset configuration
vcr_configure_reset()
