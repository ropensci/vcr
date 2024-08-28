skip_on_cran()

test_that("filter sensitive strings", {
  x <- "foo234223){@%!kl]bar"

  # vcr_c$filter_sensitive_data is NULL
  expect_null(vcr_c$filter_sensitive_data)
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove(x), x)
  
  # vcr_c$filter_sensitive_data is not NULL
  vcr_configure(
    filter_sensitive_data = list("<<my-key>>" = "234223){@%!kl]")
  )
  expect_is(vcr_c$filter_sensitive_data, "list")
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
  vcr_configure(
    filter_sensitive_data_regex = list("<<my-key>>" = "foo[0-9]+bar")
  )
  expect_is(vcr_c$filter_sensitive_data_regex, "list")
  expect_identical(sensitive_put_back(x), x)
  expect_identical(sensitive_remove("foo234223bar"), "<<my-key>>")
  # FIXME: 
  # There's no way to put back the real string unless
  # we stored it somehow, but that seems like an added security risk
  # expect_identical(sensitive_put_back(sensitive_remove(x)), x)
})

test_that("filter sensitive data strips leading/trailing single/double quotes", {
  Sys.setenv(MY_KEY_ON_GH_ACTIONS = "\"ab123c\"")
  tmpdir <- tempdir()
  vcr_configure(
    dir = tmpdir, 
    filter_sensitive_data = list("<somekey>" = Sys.getenv("MY_KEY_ON_GH_ACTIONS"))
  )
  library(crul)
  x <- HttpClient$new("https://hb.opencpu.org")
  cas <- sw(use_cassette("testing2", {
    res <- x$get("get", query = list(key = Sys.getenv("MY_KEY_ON_GH_ACTIONS")))
  }))
  int <- yaml::yaml.load_file(cas$file())$http_interactions[[1]]
  expect_false(grepl(Sys.getenv("MY_KEY_ON_GH_ACTIONS"), URLdecode(int$request$uri)))
  body <- jsonlite::fromJSON(int$response$body$string)
  expect_false(grepl(Sys.getenv("MY_KEY_ON_GH_ACTIONS"), body$args$key))
  expect_false(grepl(Sys.getenv("MY_KEY_ON_GH_ACTIONS"), body$url))

  # throws a warning when it happens
  expect_warning(use_cassette("testing3", {
    x$get("get", query = list(key = Sys.getenv("MY_KEY_ON_GH_ACTIONS")))
  }))

  # unset
  Sys.unsetenv("MY_KEY_ON_GH_ACTIONS")
})

# reset config
vcr_configure_reset()
