vcr_configure_reset()

tmpdir <- tempdir()
vcr_configure(dir = tmpdir)

test_that("quiet works", {
  library(crul)
  # default: quiet=TRUE 
  expect_true(vcr_configuration()$quiet)
  expect_message(
    use_cassette("foo3", crul::ok("https://eu.httpbin.org/get")),
    NA
  )
  expect_message(
    use_cassette("foo1", crul::ok("https://eu.httpbin.org/get")),
    NA
  )
  expect_message(
    use_cassette("foo2", crul::ok("https://eu.httpbin.org/get")),
    NA
  )
  
  # quiet=FALSE
  vcr_configure(quiet = FALSE)
  expect_false(vcr_configuration()$quiet)
  webmockr::webmockr_disable_net_connect()
  expect_message(
    use_cassette("foo3", crul::ok("https://eu.httpbin.org/get")),
    "allowed"
  )
  expect_message(
    use_cassette("foo1", crul::ok("https://eu.httpbin.org/get")),
    "enabled"
  )
  expect_message(
    use_cassette("foo2", crul::ok("https://eu.httpbin.org/get")),
    "disabled"
  )
})

vcr_configure_reset()
