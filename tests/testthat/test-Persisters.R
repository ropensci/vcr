test_that("Persisters", {
  cl <- Persisters$new()
  expect_s3_class(cl, "R6")
  expect_s3_class(cl, "Persisters")

  fs <- cl$persisters$new()
  expect_s3_class(fs, "FileSystem")
})

test_that("Persisters fails well", {
  expect_error(Persisters$new(a = 5), "unused argument")
  expect_error(
    Persisters$new(name = "bears"),
    "The requested VCR cassette persister \\(bears\\) is not registered"
  )
})
