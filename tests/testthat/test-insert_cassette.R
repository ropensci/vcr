context("insert_cassette fails well")
test_that("insert_cassette fails well", {
  unlink(file.path(vcr_c$dir, "foobar55.yml"))

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

  # update_content_length_header valid type
  expect_error(
    suppressMessages(insert_cassette("newbar3", update_content_length_header = 5)),
    "update_content_length_header must be of class logical"
  )

  # preserve_exact_body_bytes valid type
  expect_error(
    suppressMessages(insert_cassette("newbar4",  preserve_exact_body_bytes = 5)),
    "preserve_exact_body_bytes must be of class logical"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(insert_cassette("newbar5", persist_with = "foobar55")),
    "The requested VCR cassette persister \\(foobar55\\) is not registered"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(insert_cassette("newbar6", serialize_with = "howdy")),
    "The requested VCR cassette serializer \\(howdy\\) is not registered"
  )
})

unlink(list.files(vcr_c$dir, pattern = "newbar", full.names = TRUE))

context("insert_cassette works")
test_that("insert_cassette works as expected", {
  aa <- suppressMessages(insert_cassette("foobar3"))
  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "foobar3")
  expect_false(aa$allow_playback_repeats)
  expect_false(aa$any_new_recorded_interactions())
  expect_is(aa$args, "list")
  expect_is(aa$call_block, "function")

  # eject
  aa$eject()
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


# cleanup
unlink(file.path(vcr_configuration()$dir, "foobar3.yml"))
unlink(list.files(pattern = "newbar", full.names = TRUE))
unlink("foobar55.yml")
unlink("testing1.yml")
