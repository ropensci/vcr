test_that("local_cassette() integration test", {
  # This tests that everything works together AND ensures that we have
  # one cassette file on disk
  local_cassette("integration")

  req <- httr2::request(hb("/get"))
  expect_no_error(httr2::req_perform(req))
})

test_that("use_cassette returns a cassette", {
  cassette <- use_cassette("testing1", NULL, warn_on_empty = FALSE)
  expect_s3_class(cassette, "Cassette")
})

test_that("defaults to _vcr directory", {
  cassette <- use_cassette("test", NULL, warn_on_empty = FALSE)
  expect_equal(cassette$root_dir, test_path("_vcr"))
})

test_that("local_cassette() checks its inputs", {
  local_vcr_configure(warn_on_empty_cassette = FALSE)

  expect_snapshot(error = TRUE, {
    local_cassette()
    local_cassette("test")
    local_cassette("test", NULL, record = "xxx")
    local_cassette("test", NULL, match_requests_on = "xxx")
    local_cassette("text", NULL, preserve_exact_body_bytes = "xxx")
    local_cassette("test", NULL, serialize_with = "howdy")
  })
})

test_that("local_cassette checks name", {
  local_vcr_configure(warn_on_empty_cassette = FALSE)

  expect_snapshot(error = TRUE, {
    local_cassette("foo bar")
    local_cassette("foo.yml")
    local_cassette("foo/bar")
    local_cassette("foo\nbar")
    local_cassette("foo\nbar.")
    local_cassette("..")
    local_cassette("con")
    local_cassette(strrep("x", 400))
  })

  expect_snapshot(
    error = TRUE,
    local({
      local_cassette("foo")
      local_cassette("foo")
    })
  )
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

test_that("local_cassette sets env vars", {
  local_vcr_configure(dir = withr::local_tempdir())

  expect_false(is_recording())
  expect_false(is_replaying())

  local({
    local_cassette("test")
    expect_true(is_recording())
    expect_false(is_replaying())
    httr::GET(hb("/get"))
  })

  local({
    local_cassette("test")
    expect_false(is_recording())
    expect_true(is_replaying())
    httr::GET(hb("/get"))
  })
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
