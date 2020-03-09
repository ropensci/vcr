context("use_vcr works")

test_that("use_vcr works", {
  skip_on_cran()
  
  expect_is(use_vcr, "function")

  dir <- file.path(tempdir(), "foobar")
  invisible(make_pkg(dir))
  res <- use_vcr(dir, verbose = FALSE)
  expect_null(res)
  expect_true(dir.exists(file.path(dir, "tests")))
  expect_true(file.exists(file.path(dir, "tests/testthat.R")))
  expect_true(file.exists(file.path(dir, "tests/testthat/helper-foobar.R")))
  help <- paste0(readLines(file.path(dir, "tests/testthat/helper-foobar.R")),
    collapse = " ")
  expect_match(help, "vcr::vcr_configure")
  expect_match(help, "vcr::check_cassette_names")
  expect_true(file.exists(file.path(dir, "tests/testthat/test-vcr_example.R")))
  expect_true(any(grepl("vcr", readLines(file.path(dir, "DESCRIPTION")))))
  expect_true(file.exists(file.path(dir, ".gitattributes")))
  gitatts <- readLines(file.path(dir, ".gitattributes"))
  expect_true(any(grepl("text=auto", gitatts)))
  expect_true(any(grepl("tests/fixtures", gitatts)))

  # cleanup
  unlink(dir, TRUE, TRUE)
})

test_that("use_vcr fails well", {
  expect_error(use_vcr(5), "dir must be of class character")
  expect_error(use_vcr(letters[1:2]), "length\\(dir\\) == 1 is not TRUE")

  # dir does not exist
  dir <- file.path(tempdir(), "foobar2")
  expect_error(use_vcr(dir), "'dir' does not exist")

  # DESCRIPTION file does not exist
  dir2 <- file.path(tempdir(), "foobar3")
  dir.create(dir, recursive = TRUE)
  expect_error(use_vcr(dir), "'DESCRIPTION' not found")
})
