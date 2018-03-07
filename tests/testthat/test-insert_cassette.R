context("insert_cassette")

test_that("insert_cassette", {
  aa <- insert_cassette("foobar")
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "foobar")
  expect_false(aa$allow_playback_repeats)
  expect_true(aa$allow_unused_http_interactions)
  expect_true(aa$allow_unused_http_interactions)
  expect_false(aa$any_new_recorded_interactions())
  expect_is(aa$args, "list")
  expect_is(aa$call_block, "function")

  # eject
  aa$eject()
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "foobar.yml"))
