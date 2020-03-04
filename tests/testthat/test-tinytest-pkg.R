test_that("does tinytest work", {
  skip_on_cran()
  skip_if_not_installed("tinytest")
  
  dir <- file.path(tempdir(), "tinytesttest")
  invisible(make_pkg(dir, "tinytest"))
  strg_run_tests <- 'tinytest::test_package("tinytesttest")\n'
  dir.create(file.path(dir, "tests"))
  cat(strg_run_tests, file = file.path(dir, "tests/tinytest.R"))
  
  strg_test <- 'vcr::use_cassette("testing", {
crul::HttpClient$new("https://httpbin.org/get")$get()
\n'
  cat(strg_test, file = file.path(dir, "inst/tinytest/test-foo.R"))
  
  og <- getwd()
  setwd(file.path(dir, "tests/testthat"))
  on.exit(setwd(og))

  expect_error(check_cassette_names(),
    "you should not have duplicated cassette names", class = "error")
  mssg <- tryCatch(check_cassette_names(), error = function(e) e)
  expect_match(mssg$message, "you should not have duplicated cassette names")
  expect_match(mssg$message, "testing \\(found in ")
  expect_match(mssg$message, "test-catbar.R, test-single.R, test-vcr_example.R")

  # cleanup
  unlink(dir, TRUE, TRUE)
})
