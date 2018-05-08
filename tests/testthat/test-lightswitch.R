context("lightswitch functions")

test_that("turn_on & turned_on", {
  expect_true(turned_on())

  expect_false(light_switch$turned_off)
  expect_false(turn_on())
  expect_false(light_switch$turned_off)

  expect_true(turned_on())
})

test_that("turn_off", {
  expect_true(turned_on())

  expect_false(light_switch$turned_off)
  expect_true(suppressMessages(turn_off()))
  expect_message(turn_off(), "vcr turned off")
  expect_true(light_switch$turned_off)

  expect_false(turned_on())
})

test_that("turned_off", {
  turn_on()

  # if a cassette is in use
  mycas <- insert_cassette("adfadfdfadfadsf")
  expect_error(turned_off(5 + 5), "You must eject it")
  mycas$eject()

  # no cassette in use
  expect_true(turned_on())
  suppressMessages(turned_off({
    beetle <- crul::HttpClient$new(url = "https://eu.httpbin.org/get")$get()
  }))
  expect_is(beetle, "HttpResponse")
  expect_true(turned_on())
})

# cleanup
# eject_cassette()
unlink(file.path(vcr_configuration()$dir, "adfadfdfadfadsf.yml"))

# reset configuration
vcr_configure_reset()
