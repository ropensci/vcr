test_that("vcr_test_path works", {
  expect_equal(vcr_test_path("fixtures"), "../fixtures")
  withr::local_envvar(c("TESTTHAT" = "false"))
  expect_error(vcr_test_path("fixtures"), "find")
  expect_error(vcr_test_path("", "a"), "non empty")
})
