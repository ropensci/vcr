context("use_cassette options: re_record_interval")

# library(crul, quietly = TRUE)
mydir <- file.path(tempdir(), "use_cassette_re_record")
# invisible(vcr_configure(dir = mydir))
conn <- crul::HttpClient$new("https://eu.httpbin.org")
# vcr::vcr_configure(
#   dir = mydir,
#   log = TRUE, 
#   log_opts = list(file = "vcr.log", log_prefix = "Cassette", date = TRUE)
# )
yml_path <- file.path(vcr_c$dir, "re_record1.yml")

test_that("use_cassette options: re_record_interval", {
  skip_on_cran()

  unlink(file.path(vcr_c$dir, "re_record1.yml"))

  # first use
  use_cassette("re_record1", {
    res <- conn$get("get")
  },
  re_record_interval = 10L,
  clean_outdated_http_interactions = TRUE)
  rr1 <- yaml::yaml.load_file(yml_path)

  # second use, not expired, no change in recorded_at value
  use_cassette("re_record1", {
    res <- conn$get("get")
  },
  re_record_interval = 10L,
  clean_outdated_http_interactions = TRUE)
  rr2 <- yaml::yaml.load_file(yml_path)

  expect_equal(rr1$recorded_at, rr2$recorded_at)

  # third use, Sys.sleep, now expired, A change in recorded_at value
  Sys.sleep(10)
  use_cassette("re_record1", {
    res <- conn$get("get")
  },
  re_record_interval = 10L,
  clean_outdated_http_interactions = TRUE)
  rr3 <- yaml::yaml.load_file(yml_path)

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

# cleanup
unlink(file.path(vcr_c$dir, "re_record1.yml"))
# reset configuration
vcr_configure_reset()

# cas <- insert_cassette("re_record1",
#   re_record_interval = 10L,
#   clean_outdated_http_interactions = TRUE)
