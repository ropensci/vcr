context("Configuration")

teardown({
  vcr_configure_reset()
  vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))
})

test_that("VCRConfig", {
  expect_is(VCRConfig, "R6ClassGenerator")
  cl <- vcr_configuration()
  expect_is(cl,  "R6")
  expect_is(cl,  "VCRConfig")
})

test_that("config fails well with invalid record mode", {
  expect_error(
    vcr_configure(record = "asdfadfs"),
    "'record' value of 'asdfadfs' is not in the allowed set"
  )
})

test_that("config fails well with invalid request matchers", {
  expect_error(
    vcr_configure(match_requests_on = "x"),
    "1 or more 'match_requests_on' values \\(x\\) is not in the allowed set"
  )
})

test_that("config fails well with unsupported matcher", {
  expect_error(
    vcr_configure(match_requests_on = "host"),
    "we do not yet support host and path matchers"
  )
})

test_that("vcr_configure() only affects settings passed as arguments", {
  vcr_configure_reset()
  vcr_configure(dir = "olddir", record = "none")
  config1 <- vcr_c$clone()

  vcr_configure(dir = "newdir")
  config2 <- vcr_c$clone()

  expect_equal(config1$dir, "olddir")
  expect_equal(config2$dir, "newdir")

  expect_equal(config1$record, "none")
  expect_equal(config2$record, "none")
})

test_that("warnings are thrown for invalid parameters", {
  expect_warning(
    vcr_configure(foo = "bar"),
    "The following configuration parameters are not valid"
  )
})

test_that("all configuration params are documented", {
  rd_file <- "../../man/vcr_configure.Rd"
  skip_if_not(file.exists(rd_file), sprintf("Did not find: '%s'", rd_file))

  rd_args <- extract_vcr_config_args(rd_file)
  fn_args <- names(VCRConfig$new()$as_list())

  expect_setequal(rd_args, fn_args)
})
