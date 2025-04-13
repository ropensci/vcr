test_that("eject_cassette", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    warn_on_empty_cassette = FALSE
  )

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar78"))
  ej <- eject_cassette()
  expect_s3_class(ej, "Cassette")
  # expect_equal(length(ej$deserialized_hash()), 0)
  expect_false(file.exists(ej$file()))

  # eject without giving name, ejects current cassette
  invisible(insert_cassette("foobar22"))
  ej <- eject_cassette(cassette = "foobar22")
  expect_s3_class(ej, "Cassette")
  expect_false(file.exists(ej$file()))
})
