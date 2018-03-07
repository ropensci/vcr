context("cassettes")

test_that("cassettes", {
  aa <- cassettes()
  expect_is(aa, "list")
  expect_equal(length(aa), 0)

  cc <- insert_cassette("foobar")

  bb <- cassettes()
  # cassette in named list
  expect_named(bb, "foobar")
  # even after cassette inserted, list is empty
  expect_equal(length(bb$foobar), 0)

  # eject
  cc$eject()
})

unlink(file.path(vcr_configuration()$dir, "foobar.yml"))

# FIXME: add tests for on_disk and verb params
