context("eject_cassette")

vcr_configure(warn_on_empty_cassette = FALSE)

test_that("eject_cassette", {
  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar78"))
  ej <- eject_cassette()
  expect_is(ej, "Cassette")
  # expect_equal(length(ej$deserialized_hash()), 0)
  expect_false(file.exists(ej$file()))

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar22"))
  ej <- eject_cassette(cassette = "foobar22")
  expect_is(ej, "Cassette")
  expect_false(file.exists(ej$file()))
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "foobar78.yml"))
unlink(file.path(vcr_configuration()$dir, "foobar22.yml"))
