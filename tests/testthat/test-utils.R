test_that("pkg_versions", {
  expect_match(pkg_versions(), "vcr/")
  expect_match(pkg_versions(), "webmockr/")
})
