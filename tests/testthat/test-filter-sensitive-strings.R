skip_on_cran()

test_that("filter sensitive strings", {
  x <- "foo234223){@%!kl]bar"

  local_vcr_configure(filter_sensitive_data = NULL)
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove(x), x)

  # vcr_c$filter_sensitive_data is not NULL
  local_vcr_configure(
    filter_sensitive_data = list("<<my-key>>" = "234223){@%!kl]")
  )
  expect_type(vcr_c$filter_sensitive_data, "list")
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove("foo234223){@%!kl]bar"), "foo<<my-key>>bar")
  expect_identical(sensitive_put_back(sensitive_remove(x)), x)
})

test_that("filter sensitive regex strings", {
  x <- "foo234223bar"

  # vcr_c$filter_sensitive_data_regex is NULL
  expect_null(vcr_c$filter_sensitive_data_regex)
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove(x), x)

  # vcr_c$filter_sensitive_data is not NULL
  local_vcr_configure(
    filter_sensitive_data_regex = list("<<my-key>>" = "foo[0-9]+bar")
  )
  expect_type(vcr_c$filter_sensitive_data_regex, "list")
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove("foo234223bar"), "<<my-key>>")
  # FIXME:
  # There's no way to put back the real string unless
  # we stored it somehow, but that seems like an added security risk
  # expect_identical(sensitive_put_back(sensitive_remove(x)), x)
})

test_that("filter sensitive data strips leading/trailing single/double quotes", {
  withr::local_envvar(MY_KEY_ON_GH_ACTIONS = "\"ab123c\"")
  tmpdir <- withr::local_tempdir()
  local_vcr_configure(
    dir = withr::local_tempdir(),
    filter_sensitive_data = list(
      "<somekey>" = Sys.getenv("MY_KEY_ON_GH_ACTIONS")
    )
  )
  x <- crul::HttpClient$new("https://hb.opencpu.org")
  expect_snapshot(
    cas <- use_cassette(
      "testing2",
      res <- x$get(
        "get",
        query = list(key = Sys.getenv("MY_KEY_ON_GH_ACTIONS"))
      )
    )
  )
  int <- yaml::yaml.load_file(cas$file())$http_interactions[[1]]
  expect_false(grepl(
    Sys.getenv("MY_KEY_ON_GH_ACTIONS"),
    URLdecode(int$request$uri)
  ))
  body <- jsonlite::fromJSON(int$response$body$string)
  expect_false(grepl(Sys.getenv("MY_KEY_ON_GH_ACTIONS"), body$args$key))
  expect_false(grepl(Sys.getenv("MY_KEY_ON_GH_ACTIONS"), body$url))
})
