test_that("Cassette options", {
  cl <- Cassette$new(name = "stuff", re_record_interval = 1000L,
    clean_outdated_http_interactions = TRUE)
  expect_is(cl, "R6")
  expect_is(cl, "Cassette")

  expect_is(cl$re_record_interval, "integer")
  expect_equal(cl$re_record_interval, 1000L)
  expect_is(cl$clean_outdated_http_interactions, "logical")
  expect_true(cl$clean_outdated_http_interactions)

  # eject cassette
  cl$eject()
})

# cleanup
unlink(file.path(vcr_configuration()$dir, "stuff.yml"))
