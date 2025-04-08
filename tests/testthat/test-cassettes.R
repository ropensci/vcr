tmpdir <- tempdir()
vcr_configure(dir = tmpdir, warn_on_empty_cassette = FALSE)

test_that("cassettes works", {
  aa <- cassettes()
  expect_type(aa, "list")
  expect_equal(length(aa), 0)

  cc <- suppressMessages(insert_cassette("foobar24"))

  bb <- cassettes()
  # cassette in named list
  expect_named(bb, "foobar24")
  # even after cassette inserted, list is empty
  expect_equal(length(bb$foobar24), 0)

  # eject
  cc$eject()
})

unlink(file.path(vcr_configuration()$dir, "foobar24.yml"))

# FIXME: add tests for on_disk and verb params

test_that("current_cassette works", {
  # no cassettes in use
  aa <- current_cassette()
  expect_type(aa, "list")
  expect_equal(length(aa), 0)

  # cassette in use
  cas <- insert_cassette("rrrrrrrrrrr")
  aa <- current_cassette()
  expect_s3_class(aa, "Cassette")
  expect_gt(length(aa), 1)
  expect_equal(aa$name, 'rrrrrrrrrrr')
  cas$eject()
})

unlink(file.path(vcr_configuration()$dir, "rrrrrrrrrrr.yml"))

test_that("cassette_path works", {
  # before vcr_config set, there's a temp dir
  aa <- cassette_path()
  expect_type(aa, "character")

  # after vcr_config set, dir should be different
  vcr_configure(dir = "foo")
  aa <- cassette_path()
  expect_equal(aa, "foo")
})

# reset
vcr_configure_reset()
