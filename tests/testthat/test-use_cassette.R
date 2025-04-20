test_that("can control output directory directly", {
  dir1 <- withr::local_tempdir()
  dir2 <- withr::local_tempdir()

  local_vcr_configure(dir = dir1)
  local({
    local_cassette("test", dir = dir2)
    httr::GET(hb("/get"))
  })

  expect_false(file.exists(file.path(dir1, "test.yml")))
  expect_true(file.exists(file.path(dir2, "test.yml")))
})
