context("eject_cassette")

test_that("eject_cassette", {
  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar"))
  ej <- eject_cassette()
  expect_null(ej)

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar2"))
  ej <- eject_cassette(cassette = "foobar2")
  expect_null(ej)
  expect_equal(length(cassettes(FALSE)), 0)
})

# cleanup
unlink("fixtures/vcr_cassettes/foobar.yml")
unlink("fixtures/vcr_cassettes/foobar2.yml")
