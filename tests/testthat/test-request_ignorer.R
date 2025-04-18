test_that("can configure ignored hosts", {
  skip_on_cran()
  local_vcr_configure(dir = withr::local_tempdir())

  # Ingore nothing
  use_cassette("test-1", {
    crul::HttpClient$new("https://google.com")$get()
    crul::HttpClient$new("https://scottchamberlain.info")$get()
  })
  cas <- read_cassette("test-1.yml")
  expect_equal(length(cas$http_interactions), 2)

  # Ignore host
  local_vcr_configure(ignore_hosts = "google.com")
  use_cassette("test-2", {
    crul::HttpClient$new("https://google.com")$get()
    crul::HttpClient$new("https://scottchamberlain.info")$get()
  })
  cas <- read_cassette("test-2.yml")
  expect_equal(length(cas$http_interactions), 1)

  # Ignore localhost
  local_vcr_configure(ignore_localhost = TRUE)
  cas_local_ignored <- use_cassette("test-3", {
    crul::HttpClient$new("https://scottchamberlain.info")$get()
    crul::HttpClient$new(hb("/get"))$get()
  })
  cas <- read_cassette("test-3.yml")
  expect_equal(length(cas$http_interactions), 1)
})
