context("Persisters")

test_that("Persisters", {
  expect_is(Persisters, "R6ClassGenerator")
  cl <- Persisters$new()
  expect_is(cl,  "R6")
  expect_is(cl,  "Persisters")

  fs <- cl$persisters$new()
  expect_is(fs, "FileSystem")
})

test_that("Persisters fails well", {
  expect_error(Persisters$new(a = 5), "unused argument")
  expect_error(Persisters$new(name = "bears"),
          "The requested VCR cassette persister \\(bears\\) is not registered")
})
