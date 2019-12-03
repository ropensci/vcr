context("lightswitch functions")

test_that("default options set as expected", {
  expect_false(light_switch$turned_off)
  expect_false(light_switch$ignore_cassettes)
})

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

# turn back on
turn_on()

test_that("turn_off and ignore_cassettes works correctly", {
  # before turned off, insert_cassette works
  z <- insert_cassette("abcd")
  expect_is(z, "Cassette")
  z$eject()

  # after being turned off, insert_cassette throws an error
  turn_off()
  expect_error(insert_cassette("abcd"), "vcr is turned off")

  # after being turned off && ignore_cassettes=TRUE:
  # - insert_cassette returns `NULL`
  # - use_cassette returns `NULL` & block is run
  # - current_cassette returns `list()`
  turn_off(ignore_cassettes = TRUE)
  expect_null(insert_cassette("abcd"))
  uc <- use_cassette("abcd", {
    yielded <- 5
  })
  expect_null(uc)
  expect_equal(yielded, 5)
  expect_equal(length(current_cassette()), 0)
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "abcd.yml"))

# turn back on
turn_on()

test_that("lightswitch env var's", {
  # VCR_TURN_OFF not set, insert_cassette works
  expect_equal(Sys.getenv("VCR_TURN_OFF"), "")
  z <- insert_cassette("abcd")
  expect_is(z, "Cassette")
  invisible(z$eject())

  # after being turned off, insert_cassette throws an error
  Sys.setenv(VCR_TURNED_OFF=TRUE)
  Sys.setenv(VCR_IGNORE_CASSETTES=FALSE)
  expect_equal(Sys.getenv("VCR_TURN_OFF"), "")
  expect_equal(Sys.getenv("VCR_IGNORE_CASSETTES"), "FALSE")
  expect_equal(Sys.getenv("VCR_TURNED_OFF"), "TRUE")
  expect_error(insert_cassette("defg"), "vcr is turned off")

  # after both VCR_TURN_OFF && VCR_IGNORE_CASSETTES set to TRUE:
  # - insert_cassette returns `NULL`
  # - use_cassette returns `NULL` & block is run
  # - current_cassette returns `list()`
  Sys.setenv(VCR_TURN_OFF=FALSE)
  Sys.setenv(VCR_TURNED_OFF=TRUE)
  Sys.setenv(VCR_IGNORE_CASSETTES=TRUE)
  expect_null(insert_cassette("asdffd"))
  uc <- use_cassette("asdffdddd", {
    yielded <- 55
  })
  expect_null(uc)
  expect_equal(yielded, 55)
  expect_equal(length(current_cassette()), 0)
})

# reset lightswitch env vars
Sys.setenv(VCR_TURN_OFF="")
Sys.setenv(VCR_TURNED_OFF="")
Sys.setenv(VCR_IGNORE_CASSETTES="")

test_that("lightswitch env var handling fails well", {
  # various problems
  Sys.setenv(VCR_TURN_OFF=4)
  expect_error(vcr_env_handle(), "invalid option for env var")
  Sys.setenv(VCR_TURN_OFF="4")
  expect_error(vcr_env_handle(), "invalid option for env var")
  Sys.setenv(VCR_TURN_OFF="adfasdfsfd")
  expect_error(vcr_env_handle(), "invalid option for env var")

  # different boolean forms work
  Sys.setenv(VCR_TURN_OFF="true")
  expect_null(vcr_env_handle())
  Sys.setenv(VCR_TURN_OFF="FALSE")
  expect_null(vcr_env_handle())
  Sys.setenv(VCR_TURN_OFF=TRUE)
  expect_null(vcr_env_handle())
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "defg.yml"))
unlink(file.path(vcr_configuration()$dir, "asdffd.yml"))
unlink(file.path(vcr_configuration()$dir, "asdffdddd.yml"))

# reset lightswitch env vars
Sys.setenv(VCR_TURN_OFF="")
Sys.setenv(VCR_TURNED_OFF="")
Sys.setenv(VCR_IGNORE_CASSETTES="")

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
