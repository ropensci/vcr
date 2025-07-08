test_that("pkg_versions", {
  expect_match(pkg_versions(), "vcr/")
})

test_that("can set env vars", {
  withr::local_envvar(x1 = "a")

  old <- set_env_var(list(x1 = "b"))
  expect_equal(old, list(x1 = "a"))
  expect_equal(Sys.getenv("x1"), "b")

  old <- set_env_var(list(x1 = NA))
  expect_equal(old, list(x1 = "b"))
  expect_equal(Sys.getenv("x2", unset = NA), NA_character_)
})
