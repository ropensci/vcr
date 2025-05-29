test_that("helpful error if no requests match", {
  local_vcr_configure(dir = withr::local_tempdir(), record = "none")

  req <- httr2::request(hb("/get"))
  expect_snapshot(use_cassette("test", httr2::req_perform(req)), error = TRUE)

  # Make it clear that we don't get a redundant warning about the cassette
  # being empty
  use_cassette("test", httr2::req_perform(req)) |>
    expect_no_warning() |>
    expect_error(class = "vcr_unhandled")
})
