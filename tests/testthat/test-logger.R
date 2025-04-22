test_that("can locally change logging settings", {
  local_vcr_configure_log(file = "x")

  local({
    local_vcr_configure_log(file = "y")
    expect_equal(vcr_c$log_opts$file, "y")
  })

  expect_equal(vcr_c$log_opts$file, "x")
})

test_that("logging is silent unless enabled", {
  expect_silent(vcr_log_sprintf("log"))

  local_vcr_configure_log(file = stdout())
  capture.output(vcr_log_sprintf("log"))
})

test_that("vcr_log_sprintf() adds additional metadata", {
  local_mocked_bindings(Sys.time = function() as.POSIXct("2024-01-01"))

  local_vcr_configure_log(file = stdout(), include_date = FALSE)
  expect_snapshot(vcr_log_sprintf("log"))

  # Turn logging off and add a cassette
  local_vcr_configure_log(log = FALSE)
  local_cassette("testing", warn_on_empty = FALSE)

  local_vcr_configure_log(file = stdout(), include_date = TRUE)
  expect_snapshot(vcr_log_sprintf("log"))
})
