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
  on.exit({
    unlink(file.path(vcr_configuration()$dir, "aaa.yml"))
    unlink(file.path(vcr_configuration()$dir, "bbb.yml"))
  })

  # no cassettes in use
  aa <- current_cassette()
  expect_equal(aa, NULL)

  # cassette in use
  cas_a <- insert_cassette("aaa")
  expect_equal(current_cassette()$name, "aaa")

  cas_b <- insert_cassette("bbb")
  expect_equal(current_cassette()$name, "bbb")

  cas_b$eject()
  expect_equal(current_cassette()$name, "aaa")

  cas_a$eject()
  expect_equal(current_cassette(), NULL)
})

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
