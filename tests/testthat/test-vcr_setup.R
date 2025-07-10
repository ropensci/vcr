test_that("use_vcr is deprecated", {
  dir <- make_pkg()
  expect_snapshot(use_vcr(dir, verbose = FALSE))
})

test_that("use_vcr works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_type(use_vcr, "closure")

  dir <- make_pkg()
  res <- use_vcr(dir, verbose = FALSE)
  expect_null(res)
  expect_true(dir.exists(file.path(dir, "tests")))
  expect_true(file.exists(file.path(dir, "tests/testthat.R")))
  expect_true(file.exists(file.path(dir, "tests/testthat/helper-vcr.R")))
  help <- paste0(
    readLines(file.path(dir, "tests/testthat/helper-vcr.R")),
    collapse = " "
  )
  expect_match(help, "vcr::vcr_configure")
  expect_match(help, "vcr::check_cassette_names")
  expect_true(file.exists(file.path(dir, "tests/testthat/test-vcr_example.R")))
  expect_true(any(grepl("vcr", readLines(file.path(dir, "DESCRIPTION")))))
  expect_true(any(grepl(">=", readLines(file.path(dir, "DESCRIPTION")))))
  expect_true(file.exists(file.path(dir, ".gitattributes")))
  gitatts <- readLines(file.path(dir, ".gitattributes"))
  expect_true(any(grepl("text=auto", gitatts)))
  expect_true(any(grepl("tests/fixtures", gitatts)))
})

test_that("use_vcr works", {
  withr::local_options(lifecycle_verbosity = "quiet")
  expect_type(use_vcr, "closure")

  dir <- make_pkg()
  expect_snapshot(
    use_vcr(dir),
    transform = \(x) gsub(basename(dir), "{dir}", x)
  )
})

test_that("use_vcr fails well", {
  withr::local_options(lifecycle_verbosity = "quiet")

  expect_snapshot(use_vcr(5), error = TRUE)

  # dir does not exist
  dir <- "doesnt_exist"
  expect_error(use_vcr(dir), "'dir' does not exist")

  # DESCRIPTION file does not exist
  dir2 <- withr::local_tempdir()
  expect_error(use_vcr(dir2), "'DESCRIPTION' not found")
})
