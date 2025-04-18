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

test_that("insert_cassette fails well on name checking", {
  # no spaces
  expect_error(insert_cassette("foo bar"), "no spaces")

  # no file ext included - just checking yml/yaml for now
  expect_error(insert_cassette("foo.yml"), "extension")
  expect_error(insert_cassette("foo.yaml"), "extension")

  # no illegal characters
  ## no forward slash
  expect_error(insert_cassette("foo/bar"))
  ## no question marks
  expect_error(insert_cassette("foo?bar"))
  ## no left carrot
  expect_error(insert_cassette("foo<bar"))
  ## no right carrot
  expect_error(insert_cassette("foo>bar"))
  ## no backward slash
  expect_error(insert_cassette("foo\bar"))
  ## no colon
  expect_error(insert_cassette("foo:bar"))
  ## no pipe
  expect_error(insert_cassette("foo|bar"))
  ## no double quote
  expect_error(insert_cassette("foo\"bar"))

  # no control characters
  expect_error(insert_cassette("foo\nbar"))

  # no unix reserved characters
  expect_error(insert_cassette("."))
  expect_error(insert_cassette(".."))
  expect_error(insert_cassette("..."))

  # no windows reserved words
  expect_error(insert_cassette("con"))

  # no trailing dots
  expect_error(insert_cassette("foobar."))

  # can't be longer than 255
  long_name <- paste0(unlist(replicate(10, letters, FALSE)), collapse = "")
  expect_error(insert_cassette(long_name))
})
