test_that("use_cassette returns a cassette", {
  cassette <- use_cassette("testing1", NULL, warn_on_empty = FALSE)
  expect_s3_class(cassette, "Cassette")
})

test_that("use_cassette() checks its inputs", {
  expect_snapshot(error = TRUE, {
    use_cassette()
    use_cassette("test")
    use_cassette("test", NULL, record = "xxx")
    use_cassette("test", NULL, match_requests_on = "xxx")
    use_cassette("text", NULL, preserve_exact_body_bytes = "xxx")
  })
})

test_that("local_cassette sets up temporary cassette", {
  local_vcr_configure(warn_on_empty_cassette = FALSE)
  expect_equal(current_cassette(), NULL)

  local({
    local_cassette("foo")
    expect_equal(current_cassette()$name, "foo")
  })

  expect_equal(current_cassette(), NULL)
})

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
