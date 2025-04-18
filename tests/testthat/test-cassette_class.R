test_that("useful error with bad serializer", {
  expect_snapshot(
    Cassette$new("newbar6", serialize_with = "howdy"),
    error = TRUE
  )
})

test_that("cassette checks name", {
  expect_snapshot(error = TRUE, {
    Cassette$new("foo bar")
    Cassette$new("foo.yml")
    Cassette$new("foo/bar")
    Cassette$new("foo\nbar")
    Cassette$new("foo\nbar.")
    Cassette$new("..")
    Cassette$new("con")
    Cassette$new(strrep("x", 400))
  })

  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )
  local_cassette("foo")
  expect_snapshot(Cassette$new("foo"), error = TRUE)
})
