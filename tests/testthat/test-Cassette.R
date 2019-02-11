context("Cassette")

test_that("Cassette", {
  expect_is(Cassette, "R6ClassGenerator")
  cl <- Cassette$new(name = "stuff")
  expect_is(cl,  "R6")
  expect_is(cl,  "Cassette")

  # eject cassette
  cl$eject()
})

test_that("Cassette fails well", {
  expect_error(Cassette$new(), "\"name\" is missing")
})

test_that("Cassette fails well with invalid record mode", {
  expect_error(
    Cassette$new(name = "stuff2", record = "asdfadfs"),
    "'record' value of 'asdfadfs' is not in the allowed set"
  )
})

test_that("Cassette fails well with unsupported matcher", {
  expect_error(Cassette$new("foobar89", match_requests_on = "host"), 
    "we do not yet support host, path, or body matchers")
})



# cleanup
unlink(file.path(vcr_configuration()$dir, "stuff.yml"))
unlink(file.path(vcr_configuration()$dir, "foobar89.yml"))
