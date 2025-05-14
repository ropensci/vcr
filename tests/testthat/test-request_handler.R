test_that("helpful error if no requests match", {
  local_vcr_configure(dir = withr::local_tempdir(), record = "none")

  req <- httr2::request(hb("/get"))
  req2 <- httr2::request(hb("/get/2"))
  expect_snapshot(
    use_cassette("test", httr2::req_perform(req)),
    error = TRUE
  )
})
