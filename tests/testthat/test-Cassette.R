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
    "your 'record' option 'asdfadfs' was not in the allowed set"
  )
})



# cleanup
unlink(file.path(vcr_configuration()$dir, "stuff.yml"))
