test_that("use_cassette works as expected", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  aa <- use_cassette(
    "testing1",
    res <- crul::HttpClient$new(hb("/get"))$get()
  )

  # test `print.cassette` method
  expect_output(print(aa), "<vcr - Cassette>")
  expect_output(print(aa), "Record method: once")
  expect_output(print(aa), "Serialize with: yaml")
  expect_output(print(aa), "Persist with: FileSystem")
  expect_output(print(aa), "preserve_exact_body_bytes")

  expect_s3_class(aa, "Cassette")
  expect_type(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_false(aa$allow_playback_repeats)
  expect_true(aa$any_new_recorded_interactions())
  expect_type(aa$args, "list")

  expect_s3_class(res, "HttpResponse")
  expect_type(res$content, "raw")

  cas <- readLines(file.path(vcr_c$dir, "testing1.yml"))
  expect_type(cas, "character")
  expect_gt(length(cas), 10)
  expect_true(any(grepl('http_interactions', cas)))
  expect_true(any(grepl('recorded_with', cas)))
})

test_that("use_cassette fails well", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  # must pass a cassette name
  expect_snapshot(use_cassette(), error = TRUE)

  # requires a code block
  expect_snapshot(use_cassette("foobar333"), error = TRUE)

  # record valid values
  expect_error(
    use_cassette("newbar", NULL, record = "stuff"),
    "'record' value of 'stuff' is not in the allowed set"
  )

  # match_requests_on valid values
  expect_error(
    suppressMessages(use_cassette(
      "newbar",
      NULL,
      match_requests_on = "stuff"
    )),
    "'match_requests_on' values \\(stuff\\) is not in the allowed set"
  )

  # preserve_exact_body_bytes valid type
  expect_error(
    suppressMessages(use_cassette(
      "newbar4",
      NULL,
      preserve_exact_body_bytes = 5
    )),
    "preserve_exact_body_bytes must be of class logical"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(use_cassette(
      "newbar5",
      NULL,
      persist_with = "jello"
    )),
    "The requested VCR cassette persister \\(jello\\) is not registered"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(use_cassette(
      "newbar6",
      NULL,
      serialize_with = "howdy"
    )),
    "The requested vcr cassette serializer \\(howdy\\) is not registered"
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
