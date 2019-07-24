context("use_cassette: works as expected")
test_that("use_cassette works as expected", {
  skip_on_cran()

  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  aa <- use_cassette(name = "testing1", {
    res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  })

  expect_is(aa, "Cassette")
  expect_is(aa$name, "character")
  expect_equal(aa$name, "testing1")
  expect_false(aa$allow_playback_repeats)
  # expect_true(aa$any_new_recorded_interactions()) # FIXME: uncomment w/ webmockr update
  expect_is(aa$args, "list")
  expect_is(aa$call_block, "function")

  expect_is(res, "HttpResponse")
  expect_is(res$content, "raw")

  cas <- readLines(file.path(vcr_c$dir, "testing1.yml"))
  expect_is(cas, "character")
  # expect_gt(length(cas), 10) # FIXME: uncomment w/ webmockr update
  # expect_true(any(grepl('http_interactions', cas))) # FIXME: uncomment w/ webmockr update
  # expect_true(any(grepl('recorded_with', cas))) # FIXME: uncomment w/ webmockr update
})


context("use_cassette fails well")
test_that("use_cassette fails well", {

  # requires a code block
  unlink(file.path(vcr_c$dir, "foobar333.yml"))
  expect_error(
    suppressMessages(use_cassette("foobar333")),
    "`vcr::use_cassette` requires a code block"
  )

  # must pass a cassette name
  expect_error(use_cassette(), "argument \"name\" is missing")

  # record valid values
  expect_error(
    suppressMessages(use_cassette("newbar", {}, record = "stuff")),
    "'record' value of 'stuff' is not in the allowed set"
  )

  # match_requests_on valid values
  expect_error(
    suppressMessages(use_cassette("newbar", {}, match_requests_on = "stuff")),
    "'match_requests_on' values \\(stuff\\) is not in the allowed set"
  )

  # update_content_length_header valid type
  expect_error(
    suppressMessages(use_cassette("newbar3", {}, update_content_length_header = 5)),
    "update_content_length_header must be of class logical"
  )

  # preserve_exact_body_bytes valid type
  expect_error(
    suppressMessages(use_cassette("newbar4", {}, preserve_exact_body_bytes = 5)),
    "preserve_exact_body_bytes must be of class logical"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(use_cassette("newbar5", {}, persist_with = "jello")),
    "The requested VCR cassette persister \\(jello\\) is not registered"
  )

  # persist_with valid value
  expect_error(
    suppressMessages(use_cassette("newbar6", {}, serialize_with = "howdy")),
    "The requested VCR cassette serializer \\(howdy\\) is not registered"
  )
})

# cleanup
unlink(list.files(pattern = "newbar", full.names = TRUE))
unlink("foobar333.yml")
unlink("testing1.yml")

# reset configuration
vcr_configure_reset()
