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
