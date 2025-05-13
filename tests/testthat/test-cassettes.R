test_that("can insert and eject a cassette", {
  expect_false(cassette_active())
  expect_equal(current_cassette(), NULL)

  cassette <- insert_cassette("test", warn_on_empty = FALSE)
  expect_s3_class(cassette, "Cassette")
  expect_true(cassette_active())
  expect_equal(current_cassette(), cassette)

  expect_equal(eject_cassette(), cassette)

  expect_false(cassette_active())
  expect_equal(current_cassette(), NULL)
})

test_that("ejecting errors if no cassettes", {
  expect_snapshot(eject_cassette(), error = TRUE)
})

test_that("cassettes are a stack", {
  expect_equal(current_cassette(), NULL)
  expect_equal(cassettes(), list())
  expect_equal(cassette_names(), character(0))

  cassette_a <- insert_cassette("aaa", warn_on_empty = FALSE)
  expect_equal(current_cassette(), cassette_a)
  expect_equal(cassettes(), list(cassette_a))
  expect_equal(cassette_names(), "aaa")

  cassette_b <- insert_cassette("bbb", warn_on_empty = FALSE)
  expect_equal(current_cassette(), cassette_b)
  expect_equal(cassettes(), list(cassette_a, cassette_b))
  expect_equal(cassette_names(), c("aaa", "bbb"))

  eject_cassette()
  expect_equal(current_cassette(), cassette_a)
  expect_equal(cassettes(), list(cassette_a))
  expect_equal(cassette_names(), "aaa")

  eject_cassette()
  expect_equal(current_cassette(), NULL)
  expect_equal(cassettes(), list())
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
