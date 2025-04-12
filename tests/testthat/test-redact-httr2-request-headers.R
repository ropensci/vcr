test_that("request headers redacted appropriately", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("redacted_httr2", {
    httr2::request(hb("/get")) %>%
      httr2::req_headers(NotASecret = "NotHidden") %>%
      httr2::req_headers_redacted(SecretHeader = "Hidden") %>%
      httr2::req_perform()
  })

  cas <- read_cassette("redacted_httr2.yml")
  headers <- cas$http_interactions[[1]]$request$headers
  expect_equal(headers$NotASecret, "NotHidden")
  expect_equal(headers$SecretHeader, "<redacted>")
})

test_that("redact request headers doesnt do anything with httr", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("no_redaction_httr", {
    httr::GET(
      hb("/get"),
      httr::add_headers(SomeHeader = "NotHidden")
    )
  })

  cas <- read_cassette("no_redaction_httr.yml")
  expect_equal(
    cas$http_interactions[[1]]$request$headers$SomeHeader,
    "NotHidden"
  )
})

test_that("redact request headers doesnt do anything with crul", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("no_redaction_crul", {
    crul::HttpClient$new(
      hb("/get"),
      headers = list(SomeHeader = "NotHidden")
    )$get()
  })

  cas <- read_cassette("no_redaction_crul.yml")
  expect_equal(
    cas$http_interactions[[1]]$request$headers$SomeHeader,
    "NotHidden"
  )
})
