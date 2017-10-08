context("Cassette")

test_that("Cassette", {
  expect_is(Cassette, "R6ClassGenerator")
  cl <- Cassette$new(name = "stuff", new_recording = TRUE)
  expect_is(cl,  "R6")
  expect_is(cl,  "Cassette")
})

test_that("Cassette fails well", {
  expect_error(Cassette$new(), "\"name\" is missing")
  expect_error(Cassette$new(name = "stuff"), "\"new_recording\" is missing")
})

# cleanup
unlink("fixtures/vcr_cassettes/stuff.yml")
