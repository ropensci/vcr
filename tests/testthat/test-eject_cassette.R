context("eject_cassette")

test_that("eject_cassette", {
  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar78"))
  ej <- eject_cassette()
  expect_is(ej, "Cassette")
  expect_equal(length(ej$deserialized_hash()), 0)

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar22"))
  ej <- eject_cassette(cassette = "foobar22")
  expect_is(ej, "Cassette")
  expect_equal(length(ej$deserialized_hash()), 0)
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "foobar78.yml"))
unlink(file.path(vcr_configuration()$dir, "foobar22.yml"))
