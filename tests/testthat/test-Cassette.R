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

# cleanup
unlink(file.path(vcr_configuration()$dir, "stuff.yml"))
