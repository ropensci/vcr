test_that("vcr_test_path works", {
  skip_on_cran()

  # setup
  dir <- file.path(tempdir(), "bunny")
  invisible(make_pkg(dir))
  dir.create(file.path(dir, "tests"), recursive = TRUE)
  withr::local_dir(dir)

  # test
  expect_message(pth <- vcr_test_path("fixtures"), "creating", fixed = TRUE)
  expect_match(pth, "tests/fixtures", fixed = TRUE)
  expect_match(
    list.dirs(file.path(dir, "tests"), FALSE, FALSE),
    "fixtures",
    fixed = TRUE
  )
  expect_error(vcr_test_path("", "a"), "non empty", fixed = TRUE)
  expect_error(vcr_test_path(), "provide", fixed = TRUE)

  # cleanup
  unlink(dir, TRUE, TRUE)
})
