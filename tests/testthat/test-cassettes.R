test_that("cassettes works", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  expect_equal(cassettes(), list())

  insert_cassette("foobar24")
  withr::defer(eject_cassette())
  expect_s3_class(cassettes()$foobar24, "Cassette")
})

# FIXME: add tests for on_disk and verb params

test_that("current_cassette works", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

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
  local_vcr_configure(dir = withr::local_tempdir())
  # before vcr_config set, there's a temp dir
  aa <- cassette_path()
  expect_type(aa, "character")

  # after vcr_config set, dir should be different
  local_vcr_configure(dir = "foo")
  aa <- cassette_path()
  expect_equal(aa, "foo")
})
