context("Configuration")

teardown({
  vcr_configure_reset()
  vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))
})

test_that("VCRConfig", {
  expect_is(vcr::VCRConfig, "R6ClassGenerator")
  cl <- vcr_configuration()
  expect_is(cl,  "R6")
  expect_is(cl,  "VCRConfig")
})

test_that("config fails well with invalid record mode", {
  expect_error(
    vcr_configure(record = "asdfadfs"),
    "'record' value of 'asdfadfs' is not in the allowed set"
  )
})

test_that("config fails well with invalid request matchers", {
  expect_error(
    vcr_configure(match_requests_on = "x"),
    "1 or more 'match_requests_on' values \\(x\\) is not in the allowed set"
  )
})

test_that("config fails well with unsupported matcher", {
  expect_error(
    vcr_configure(match_requests_on = "host"),
    "we do not yet support host and path matchers"
  )
})
