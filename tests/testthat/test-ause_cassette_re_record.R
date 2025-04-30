conn <- crul::HttpClient$new(hb())

test_that("use_cassette options: re_record_interval", {
  local_vcr_configure(dir = withr::local_tempdir())

  # first use
  use_cassette(
    "re_record1",
    res <- conn$get("get"),
    re_record_interval = 3L,
    clean_outdated_http_interactions = TRUE
  )
  rr1 <- read_cassette("re_record1.yml")

  # second use, not expired, no change in recorded_at value
  use_cassette(
    "re_record1",
    res <- conn$get("get"),
    re_record_interval = 3L,
    clean_outdated_http_interactions = TRUE
  )
  rr2 <- read_cassette("re_record1.yml")

  expect_equal(rr1$recorded_at, rr2$recorded_at)

  # third use, Sys.sleep, now expired, A change in recorded_at value
  Sys.sleep(3L)
  use_cassette(
    "re_record1",
    res <- conn$get("get"),
    re_record_interval = 3L,
    clean_outdated_http_interactions = TRUE
  )
  rr3 <- read_cassette("re_record1.yml")

  # tests
  # expect_equal(rr1$http_interactions[[1]]$re_record_interval, 10)
  # expect_true(rr1$http_interactions[[1]]$clean_outdated_http_interactions)

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
