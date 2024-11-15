test_that("use_cassette: match_requests_on - body works w/ crul", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  cli <- HttpClient$new(url = hb())

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

  ### matchers: host only (note how query is ignored)
  # run it
  aa <- use_cassette(name = "testing_host1", {
    res <- HttpClient$new(url = hb())$get(query = list(b=99999))
  }, match_requests_on = "host")
  # run it again
  bb <- use_cassette(name = "testing_host1", {
    res2 <- HttpClient$new(url = hb())$get(query = list(a=5))
  }, match_requests_on = "host")
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing_host1")
  expect_equal(aa$match_requests_on, "host")

  ### matchers: path only (note how host and query differences are ignored)
  # run it
  aa <- use_cassette(name = "testing_path1", {
    res <- HttpClient$new("https://scottchamberlain.info")$get(
      "about", query = list(b=99999))
  }, match_requests_on = "path")
  # run it again
  bb <- use_cassette(name = "testing_path1", {
    res2 <- HttpClient$new("https://ropensci.org")$get(
      "about", query = list(a=5))
  }, match_requests_on = "path")
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing_path1")
  expect_equal(aa$match_requests_on, "path")

  ### matchers: host and path only (notice how HTTP method and query are ignored)
  # run it
  aa <- use_cassette(name = "testing_host_path", {
    res <- HttpClient$new(url = "https://ropensci.org")$get(
      "about", query = list(b=99999))
  }, match_requests_on = c("host", "path"))
  # run it again
  bb <- use_cassette(name = "testing_host_path", {
    res2 <- HttpClient$new(url = "https://ropensci.org")$post(
      "about", query = list(a=5))
  }, match_requests_on = c("host", "path"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing_host_path")
  expect_equal(aa$match_requests_on, c("host", "path"))

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
    res <- POST(hb("/post"), body = list(foo = "bar"))
  }, match_requests_on = c("method", "uri", "body"))
  # run it again
  bb <- use_cassette(name = "testing2", {
    res <- POST(hb("/post"), body = list(foo = "bar"))
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
    res <- POST(hb("/post"), query = list(a = 5), body = list(foo = "bar"))
  }, match_requests_on = c("method", "body"))
  # run it again
  bb <- use_cassette(name = "testing4", {
    res <- POST(hb("/post"), query = list(b = 2), body = list(foo = "bar"))
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
    res <- POST(hb("/post"), body = list(foo = "bar"))
  }, match_requests_on = "body")
  # run it again, method and uri changed
  bb <- use_cassette(name = "testing5", {
    res <- PUT(hb("/put"), body = list(foo = "bar"))
  }, match_requests_on = "body")
  # run it again, method and uri changed again
  cc <- use_cassette(name = "testing5", {
    res <- PATCH(hb("/patch"), body = list(foo = "bar"))
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

  ### matchers: host and path only (notice how HTTP method and query are ignored)
  # run it
  aa <- use_cassette(name = "testing_httr_host_path", {
    res <- GET("https://ropensci.org/about", query = list(b=99999))
  }, match_requests_on = c("host", "path"))
  # run it again
  bb <- use_cassette(name = "testing_httr_host_path", {
    res2 <- POST("https://ropensci.org/about", query = list(a=5))
  }, match_requests_on = c("host", "path"))
  # the recorded_at time doesn't change
  # - that is, the request matched and the recorded response in aa
  # - was used
  expect_identical(recorded_at(aa), recorded_at(bb))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing_httr_host_path")
  expect_equal(aa$match_requests_on, c("host", "path"))

  # cleanup
  unlink(mydir, recursive = TRUE)
})

# cleanup
# reset configuration
vcr_configure_reset()
