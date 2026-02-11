test_that("use_cassette options: re_record_interval", {
  local_vcr_configure(
    dir = withr::local_tempdir(),
    re_record_interval = 3L
  )

  # first use
  # use_cassette("test", res <- conn$get("get"))
  use_cassette("test", {
    conn <- crul::HttpClient$new(hb())
    res <- conn$get("get")
  })
  rr1 <- read_cassette("test.yml")

  # second use, not expired, no change in recorded_at value
  use_cassette("test", res <- conn$get("get"))
  rr2 <- read_cassette("test.yml")

  expect_equal(rr1$recorded_at, rr2$recorded_at)

  # third use, Sys.sleep, now expired, A change in recorded_at value
  Sys.sleep(3L)
  local_vcr_configure_log(file = stdout())
  expect_snapshot(
    use_cassette("test", res <- conn$get("get")),
    transform = function(x) {
      x |>
        gsub(hb(), "{httpbin}", x = _, fixed = TRUE) |>
        gsub("\\d+ bytes", "{bytes} bytes", x = _)
    }
  )
  rr3 <- read_cassette("test.yml")

  ## 1st and 2nd should be identical
  expect_true(
    rr1$http_interactions[[1]]$recorded_at ==
      rr2$http_interactions[[1]]$recorded_at
  )

  ## 1st and 3rd should be different
  expect_true(
    rr1$http_interactions[[1]]$recorded_at <
      rr3$http_interactions[[1]]$recorded_at
  )
})
