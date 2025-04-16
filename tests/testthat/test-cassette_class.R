test_that("useful error with bad serializer", {
  expect_snapshot(
    Cassette$new("newbar6", serialize_with = "howdy"),
    error = TRUE
  )
})
