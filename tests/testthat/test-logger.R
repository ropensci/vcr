test_that("vcr_log_file fails well", {
  # must pass a file name
  expect_error(vcr_log_file(), "argument \"file\" is missing")

  # file name type
  expect_error(
    vcr_log_file(5),
    "file must be of class character"
  )

  # match_requests_on valid values
  expect_error(
    vcr_log_file("adsf", overwrite = 5),
    "overwrite must be of class logical"
  )
})

test_that("vcr_log_file works as expected", {
  withr::defer(unlink("adsf"))
  expect_true(vcr_log_file("adsf"))
})

test_that("vcr_log_file: console", {
  aa <- vcr_log_file("console")

  expect_true(aa)
  expect_type(vcr_log_env, "environment")
  expect_equal(vcr_log_env$file, "console")
})

test_that("vcr_log_write: console", {
  local_vcr_configure(log = TRUE)

  aa <- vcr_log_file("console")
  expect_true(aa)

  expect_output(vcr_log_write("stuff"), "stuff")
})

test_that("vcr_log_info: console", {
  local_vcr_configure(log = TRUE)
  aa <- vcr_log_file("console")
  expect_true(aa)

  # with date time stamp
  log_date <- capture.output(vcr_log_info("stuff"))

  expect_match(log_date, "Cassette")
  expect_match(log_date, "<none>")
  expect_match(log_date, "stuff")
  expect_match(log_date, as.character(format(Sys.Date(), "%Y")))
  expect_match(log_date, "[0-9]{2}:[0-9]{2}:[0-9]{2}")

  # w/o date time stamp
  log_nodate <- capture.output(vcr_log_info("stuff", FALSE))

  expect_match(log_nodate, "Cassette")
  expect_match(log_nodate, "<none>")
  expect_match(log_nodate, "stuff")
  expect_false(grepl(as.character(format(Sys.Date(), "%Y")), log_nodate))
  expect_false(grepl("[0-9]{2}:[0-9]{2}:[0-9]{2}", log_nodate))
})
