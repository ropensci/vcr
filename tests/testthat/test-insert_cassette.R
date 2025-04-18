test_that("insert_cassette fails well", {
  local_vcr_configure(dir = withr::local_tempdir())

  # must pass a cassette name
  expect_error(insert_cassette(), "argument \"name\" is missing")

  # record valid values
  expect_error(
    suppressMessages(insert_cassette("newbar", record = "stuff")),
    "'record' value of 'stuff' is not in the allowed set"
  )

  # match_requests_on valid values
  expect_error(
    suppressMessages(insert_cassette("newbar", match_requests_on = "stuff")),
    "'match_requests_on' values \\(stuff\\) is not in the allowed set"
  )

  # preserve_exact_body_bytes valid type
  expect_error(
    suppressMessages(insert_cassette("newbar4", preserve_exact_body_bytes = 5)),
    "preserve_exact_body_bytes must be of class logical"
  )
})

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
