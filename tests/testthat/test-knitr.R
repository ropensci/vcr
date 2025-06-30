test_that("checks input type", {
  hook <- vcr_hook("", withr::local_tempdir())

  expect_snapshot(error = TRUE, {
    hook(TRUE, list(cassette = 1))
  })
})
