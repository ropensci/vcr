test_that("check_cassette_names is deprecated", {
  dir <- make_pkg()
  withr::local_dir(file.path(dir, "tests/testthat"))
  expect_snapshot(check_cassette_names())
})

test_that("check_cassette_names", {
  withr::local_options(lifecycle_verbosity = "quiet")

  test_path <- file.path(make_pkg(), "tests/testthat")
  writeLines(
    c(
      'test_that("bar", {',
      '  vcr::use_cassette("testing", cat("bar"))',
      '  vcr::use_cassette("testing", cat("bar"))',
      '})'
    ),
    file.path(test_path, "test-catbar.R")
  )
  writeLines(
    c(
      'test_that("bar", {',
      '  vcr::use_cassette("testing", cat("bar"))',
      '})'
    ),
    file.path(test_path, "test-single.R")
  )
  writeLines("hi", con = file.path(test_path, "cat.R"))

  withr::local_dir(test_path)
  expect_snapshot(check_cassette_names(), error = TRUE)

  # can allow duplicated names via the allowed_duplicates param
  expect_no_error(check_cassette_names(allowed_duplicates = "testing"))
})
