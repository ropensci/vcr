test_that("vcr_test_path works with testthat", {
  skip_on_cran()

  # setup
  dir <- withr::local_tempdir()
  dir_create(file.path(dir, "tests", "testthat"))
  withr::local_dir(dir)

  # test
  expect_message(pth <- vcr_test_path("fixtures"), "creating", fixed = TRUE)
  expect_match(pth, file.path("tests", "fixtures"), fixed = TRUE)
  expect_match(
    list.dirs(file.path(dir, "tests"), full.names = FALSE, recursive = FALSE),
    "fixtures",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("vcr_test_path works with testthat in a dir that isn't `tests`", {
  skip_on_cran()

  # setup
  dir <- withr::local_tempdir()
  dir_create(file.path(dir, "testthat"))
  withr::local_dir(dir)

  # test
  expect_message(pth <- vcr_test_path("fixtures"), "creating", fixed = TRUE)
  expect_match(pth, file.path("fixtures"), fixed = TRUE)
  expect_match(
    list.dirs(file.path(dir), full.names = FALSE, recursive = FALSE),
    "fixtures",
    fixed = TRUE,
    all = FALSE
  )
})

test_that("vcr_test_path errors with wrongly specified paths", {
  skip_on_cran()

  ## Paths may not be empty strings
  expect_error(vcr_test_path("", "a"), "non empty", fixed = TRUE)
  ## User must provide a dir name
  expect_error(vcr_test_path(), "provide", fixed = TRUE)
})
