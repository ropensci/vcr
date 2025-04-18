test_that("write_header", {
  f <- withr::local_tempfile()
  write_header(f)
  expect_equal(readLines(f), "http_interactions:")
})

test_that("dedup_keys", {
  # no modification
  x <- list(b = "foo", a = 5)
  expect_equal(dedup_keys(x), x)

  # modification: group the a keys
  x <- list(b = "foo", a = 5, a = 6)
  expect_equal(dedup_keys(x), list(a = c(5, 6), b = "foo"))

  # FIXME: doesn't yet work for nested duplicates. not sure if
  # we need it to work for this case or not?
  x <- list(b = "foo", c = list(a = 5, a = 6))
  expect_equal(dedup_keys(x), x)
})

test_that("pkg_versions", {
  expect_match(pkg_versions(), "vcr/")
  expect_match(pkg_versions(), "webmockr/")
})

test_that("get_body", {
  expect_equal(get_body(NULL), "")
  expect_equal(get_body(""), "")
  expect_equal(get_body("adsf"), "adsf")
})
