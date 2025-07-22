test_that("can turn off and turn on", {
  local_light_switch()

  expect_snapshot(turn_off())
  expect_false(turned_on())

  turn_on()
  expect_true(turned_on())
})

test_that("turned_off works if and only if no cassettes active", {
  expect_no_error(turned_off({}))

  local({
    local_cassette("test", warn_on_empty = FALSE)
    expect_snapshot(turned_off(5 + 5), error = TRUE)
  })
})

test_that("skip_if_vcr_off works", {
  local_light_switch()

  expect_no_condition(skip_if_vcr_off(), class = "skip")

  suppressMessages(turn_off())
  expect_condition(skip_if_vcr_off(), class = "skip")
})

test_that("inserting a cassette errors when vcr turned off and ignore_cassettes=FALSE", {
  local_vcr_configure(warn_on_empty_cassette = FALSE)
  local_light_switch()

  # after being turned off, insert_cassette throws an error
  suppressMessages(turn_off())
  expect_snapshot(insert_cassette("test"), error = TRUE)

  suppressMessages(turn_off(ignore_cassettes = TRUE))
  expect_no_error(insert_cassette("test", warn_on_empty = FALSE))
})


# env vars ---------------------------------------------------------------------

test_that("default options set as expected", {
  withr::local_envvar(
    VCR_TURNED_OFF = NA,
    VCR_IGNORE_CASSETTES = NA,
    VCR_TURN_OFF = NA
  )

  expect_equal(
    lightswitch_init(),
    list(on = TRUE, ignore_cassettes = FALSE)
  )
})

test_that("lightswitch_init() respects env vars", {
  expect_equal(
    lightswitch_init(),
    list(on = TRUE, ignore_cassettes = FALSE)
  )

  withr::local_envvar(VCR_TURNED_OFF = TRUE, VCR_IGNORE_CASSETTES = FALSE)
  expect_equal(
    lightswitch_init(),
    list(on = FALSE, ignore_cassettes = FALSE)
  )

  withr::local_envvar(
    VCR_TURN_OFF = FALSE,
    VCR_TURNED_OFF = TRUE,
    VCR_IGNORE_CASSETTES = TRUE
  )
  expect_equal(
    lightswitch_init(),
    list(on = FALSE, ignore_cassettes = TRUE)
  )
})

test_that("lightswitch env var handles varying inputs", {
  withr::local_envvar(VCR_TURN_OFF = "true")
  expect_equal(lightswitch_init()$on, FALSE)

  withr::local_envvar(VCR_TURN_OFF = "TRUE")
  expect_equal(lightswitch_init()$on, FALSE)

  withr::local_envvar(VCR_TURN_OFF = "yes")
  expect_snapshot(lightswitch_init(), error = TRUE)
})
