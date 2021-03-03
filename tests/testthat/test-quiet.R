# reset before tests
vcr_configure_reset()


tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("quiet works", {
  library(crul)
  # quiet=FALSE
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
  # quiet=TRUE
  expect_message(
    use_cassette("foo3", crul::ok("https://eu.httpbin.org/get"),
      quiet = TRUE),
    NA
  )
  expect_message(
    use_cassette("foo1", crul::ok("https://eu.httpbin.org/get"),
      quiet = TRUE),
    NA
  )
  expect_message(
    use_cassette("foo2", crul::ok("https://eu.httpbin.org/get"),
      quiet = TRUE),
    NA
  )

  # setting quiet via configure
  expect_false(vcr_configuration()$quiet)
  vcr_configure(quiet = TRUE)  
  expect_true(vcr_configuration()$quiet)
})

# reset
vcr_configure_reset()
