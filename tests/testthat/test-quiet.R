vcr_configure_reset()

tmpdir <- tempdir()
vcr_configure(dir = tmpdir)

test_that("quiet works", {
  library(crul)
  con <- HttpClient$new(hb())
  # default: quiet=TRUE 
  expect_true(vcr_configuration()$quiet)
  expect_message(
    use_cassette("foo3", con$get("get")),
    NA
  )
  expect_message(
    use_cassette("foo1", con$get("get")),
    NA
  )
  expect_message(
    use_cassette("foo2", con$get("get")),
    NA
  )
  
  # quiet=FALSE
  vcr_configure(quiet = FALSE)
  expect_false(vcr_configuration()$quiet)
  webmockr::webmockr_disable_net_connect()
  expect_message(
    use_cassette("foo3", con$get("get")),
    "allowed"
  )
  expect_message(
    use_cassette("foo1", con$get("get")),
    "enabled"
  )
  expect_message(
    use_cassette("foo2", con$get("get")),
    "disabled"
  )
})

vcr_configure_reset()
