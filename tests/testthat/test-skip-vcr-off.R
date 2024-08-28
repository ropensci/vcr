test_that("skip_if_vcr_off works when vcr on", {
  expect_silent(skip_if_vcr_off())
})

test_that("skip_if_vcr_off works when vcr off", {
  withr::local_envvar("VCR_TURN_OFF" = TRUE)
  expect_condition(skip_if_vcr_off(), class = "skip")
})

