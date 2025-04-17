test_that("VCRConfig", {
  cl <- vcr_configuration()
  expect_s3_class(cl, "R6")
  expect_s3_class(cl, "VCRConfig")
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

test_that("returns previous values", {
  local_vcr_configure(dir = "dir1", record = "none")

  old <- vcr_configure(dir = "dir2", record = "once")
  expect_equal(old, list(dir = "dir1", record = "none"))
})

test_that("supports !!!", {
  local_vcr_configure()

  local_vcr_configure(!!!list(dir = "dir1"))
  expect_equal(vcr_c$dir, "dir1")
})

test_that("if called with no args returns a list of all args", {
  out <- vcr_configure()
  expect_equal(out, vcr_c$as_list())
})

test_that("vcr_configure() only affects settings passed as arguments", {
  local_vcr_configure(dir = "olddir", record = "none")
  config1 <- vcr_c$clone()

  vcr_configure(dir = "newdir")
  config2 <- vcr_c$clone()

  expect_equal(config1$dir, "olddir")
  expect_equal(config2$dir, "newdir")

  expect_equal(config1$record, "none")
  expect_equal(config2$record, "none")
})

test_that("warnings are thrown for invalid parameters", {
  local_vcr_configure()

  expect_warning(
    vcr_configure(foo = "bar"),
    "The following configuration parameters are not valid"
  )
})

test_that("can temporarily change configuration", {
  local_vcr_configure(dir = "a")

  local({
    local_vcr_configure(dir = "b")
    expect_equal(cassette_path(), "b")
  })

  expect_equal(cassette_path(), "a")
})

test_that("all configuration params are documented", {
  rd_file <- "../../man/vcr_configure.Rd"
  skip_if_not(file.exists(rd_file), sprintf("Did not find: '%s'", rd_file))

  rd_args <- extract_vcr_config_args(rd_file)
  fn_args <- names(VCRConfig$new()$as_list())

  expect_setequal(rd_args, fn_args)
})
