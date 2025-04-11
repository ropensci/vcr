local_vcr_configure(dir = withr::local_tempdir())

test_that("request headers not redacted when not needed", {
  use_cassette("not_redacted_correctly", {
    httr2::request(hb("/get")) %>%
      httr2::req_headers(PublicHeader = "Not hidden") %>%
      httr2::req_perform()
  })

  cas <- read_cassette("not_redacted_correctly.yml")
  expect_equal(
    cas$http_interactions[[1]]$request$headers$PublicHeader,
    "Not hidden"
  )
})

test_that("redact request headers works when needed", {
  use_cassette("redacted_correctly", {
    httr2::request(hb("/get")) %>%
      httr2::req_headers_redacted(
        SecretHeader = "Hidden",
        AnotherSecret = "SecretKey"
      ) %>%
      httr2::req_perform()
  })

  cas <- read_cassette("redacted_correctly.yml")
  headers <- cas$http_interactions[[1]]$request$headers
  for (header in headers) {
    expect_equal(header, "<redacted>")
  }
})

test_that("redact request headers doesnt do anything with httr", {
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
