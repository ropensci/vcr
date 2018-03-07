context("eject_cassette")

test_that("eject_cassette", {
  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar"))
  ej <- eject_cassette()
  expect_is(ej, "Cassette")
  expect_equal(length(ej$deserialized_hash()), 0)

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar2"))
  ej <- eject_cassette(cassette = "foobar2")
  expect_is(ej, "Cassette")
  expect_equal(length(ej$deserialized_hash()), 0)
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "foobar.yml"))
unlink(file.path(vcr_configuration()$dir, "foobar2.yml"))
