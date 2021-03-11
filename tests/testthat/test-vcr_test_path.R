test_that("vcr_test_path works", {
  expect_match(vcr_test_path("fixtures"), "../fixtures")
  withr::local_envvar(c("TESTTHAT" = "false"))
  # expect_match(vcr_test_path("fixtures"), "tests/fixtures")
  expect_message(vcr_test_path("fixtures"), "creating")
  expect_error(vcr_test_path("", "a"), "non empty")
})
