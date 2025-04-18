test_that("quiet works", {
  con <- crul::HttpClient$new(hb())

  tmpdir <- withr::local_tempdir()
  local_vcr_configure(dir = tmpdir)

  # default: quiet=TRUE
  expect_true(vcr_configuration()$quiet)
  expect_message(use_cassette("foo3", con$get("get")), NA)
  expect_message(use_cassette("foo1", con$get("get")), NA)
  expect_message(use_cassette("foo2", con$get("get")), NA)
})
