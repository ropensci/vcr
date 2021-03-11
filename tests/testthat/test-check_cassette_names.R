test_that("check_cassette_names", {
  skip_on_cran()
  
  dir <- file.path(tempdir(), "barfly")
  invisible(make_pkg(dir))
  res <- use_vcr(dir, verbose = FALSE)
  strg_dup <- 'test_that("bar", {\n  vcr::use_cassette("testing", cat("bar"))\n  vcr::use_cassette("testing", cat("bar"))\n})'
  cat(strg_dup, file = file.path(dir, "tests/testthat/test-catbar.R"))
  strg_single <- 'test_that("bar", {\n  expect_true(TRUE)\n  vcr::use_cassette("testing", cat("bar"))\n})'
  cat(strg_single, file = file.path(dir, "tests/testthat/test-single.R"))
  strg_not_used <- 'hi'
  cat(strg_not_used, file = file.path(dir, "tests/testthat/cat.R"))
  
  og <- getwd()
  setwd(file.path(dir, "tests/testthat"))
  on.exit(setwd(og))

  expect_error(check_cassette_names(),
    "you should not have duplicated cassette names", class = "error")
  mssg <- tryCatch(check_cassette_names(), error = function(e) e)
  expect_match(mssg$message, "you should not have duplicated cassette names")
  expect_match(mssg$message, "testing \\(found in ")
  expect_match(mssg$message, "test-catbar.R, test-single.R, test-vcr_example.R")

  # can allow duplicated names via the allowed_duplicates param
  expect_error(check_cassette_names(allowed_duplicates = "testing"),
    NA)

  # cleanup
  unlink(dir, TRUE, TRUE)
})
