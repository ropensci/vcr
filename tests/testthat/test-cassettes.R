test_that("cassettes works", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  expect_equal(cassettes(), set_names(list()))

  x <- insert_cassette("x")
  withr::defer(eject_cassette())
  expect_equal(cassettes(), list(x = x))
})

# FIXME: add tests for on_disk and verb params

test_that("cassettes are a stack", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  aa <- current_cassette()
  expect_equal(aa, NULL)

  insert_cassette("aaa")
  expect_equal(current_cassette()$name, "aaa")

  insert_cassette("bbb")
  expect_equal(current_cassette()$name, "bbb")

  eject_cassette()
  expect_equal(current_cassette()$name, "aaa")

  eject_cassette()
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
