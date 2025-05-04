test_that("check_cassette_names is deprecated", {
  dir <- make_pkg()
  expect_snapshot(check_cassette_names(dir))
})

test_that("check_cassette_names", {
  withr::local_options(lifecycle_verbosity = "quiet")
  dir <- make_pkg()

  res <- use_vcr(dir, verbose = FALSE)
  strg_dup <- 'test_that("bar", {\n  vcr::use_cassette("testing", cat("bar"))\n  vcr::use_cassette("testing", cat("bar"))\n})'
  cat(strg_dup, file = file.path(dir, "tests/testthat/test-catbar.R"))
  strg_single <- 'test_that("bar", {\n  expect_true(TRUE)\n  vcr::use_cassette("testing", cat("bar"))\n})'
  cat(strg_single, file = file.path(dir, "tests/testthat/test-single.R"))
  strg_not_used <- 'hi'
  cat(strg_not_used, file = file.path(dir, "tests/testthat/cat.R"))

  withr::local_dir(file.path(dir, "tests/testthat"))
  expect_error(
    check_cassette_names(),
    "you should not have duplicated cassette names",
    class = "error"
  )
  mssg <- tryCatch(check_cassette_names(), error = function(e) e)
  expect_match(mssg$message, "you should not have duplicated cassette names")
  expect_match(mssg$message, "testing \\(found in ")
  expect_match(mssg$message, "test-catbar.R, test-single.R, test-vcr_example.R")

  # can allow duplicated names via the allowed_duplicates param
  expect_error(check_cassette_names(allowed_duplicates = "testing"), NA)
})
