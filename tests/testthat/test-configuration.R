test_that("config checks inputs", {
  expect_snapshot(error = TRUE, {
    vcr_configure(foo = "bar")
    vcr_configure(record = "asdfadfs")
    vcr_configure(match_requests_on = "x")
  })
})

test_that("returns previous values", {
  local_vcr_configure(dir = NULL, record = "none")

  old <- vcr_configure(dir = "dir1", record = "once")
  expect_equal(old, list(dir = NULL, record = "none"))
})

test_that("if called with no args returns a list of all args", {
  out <- vcr_configure()
  expect_equal(out, the$config)
})

test_that("vcr_configure() only affects settings passed as arguments", {
  local_vcr_configure(dir = "olddir", record = "none")

  config1 <- the$config
  vcr_configure(dir = "newdir")
  config2 <- the$config

  expect_equal(config1$dir, "olddir")
  expect_equal(config2$dir, "newdir")

  expect_equal(config1$record, "none")
  expect_equal(config2$record, "none")
})


test_that("can temporarily change configuration", {
  local_vcr_configure(dir = "a")

  local({
    local_vcr_configure(dir = "b")
    expect_equal(cassette_path(), "b")
  })

  expect_equal(cassette_path(), "a")
})

test_that("filter_sensitive data strips quotes with message", {
  local_vcr_configure()

  expect_snapshot(vcr_configure(filter_sensitive_data = list("key" = '"val"')))
  expect_equal(vcr_configuration()$filter_sensitive_data, list("key" = "val"))
})
