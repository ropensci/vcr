test_that("insert_cassette works as expected", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  aa <- insert_cassette("foobar3")
  withr::defer(eject_cassette())
  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "foobar3")
  expect_false(aa$allow_playback_repeats)
  expect_false(aa$any_new_recorded_interactions())
})
