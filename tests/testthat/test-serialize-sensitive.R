test_that("filter sensitive strings", {
  x <- "foo234223){@%!kl]bar"

  local_vcr_configure(filter_sensitive_data = NULL)
  expect_identical(decode_sensitive(x), x)
  expect_identical(encode_sensitive(x), x)

  # filter_sensitive_data is not NULL
  local_vcr_configure(
    filter_sensitive_data = list("<<my-key>>" = "234223){@%!kl]")
  )
  expect_type(the$config$filter_sensitive_data, "list")
  expect_identical(decode_sensitive(x), x)
  expect_identical(encode_sensitive("foo234223){@%!kl]bar"), "foo<<my-key>>bar")
  expect_identical(decode_sensitive(encode_sensitive(x)), x)
})

test_that("filter sensitive regex strings", {
  x <- "foo234223bar"

  # filter_sensitive_data_regex is NULL
  expect_null(the$config$filter_sensitive_data_regex)
  expect_identical(decode_sensitive(x), x)
  expect_identical(encode_sensitive(x), x)

  # filter_sensitive_data is not NULL
  local_vcr_configure(
    filter_sensitive_data_regex = list("<<my-key>>" = "foo[0-9]+bar")
  )
  expect_type(the$config$filter_sensitive_data_regex, "list")
  expect_identical(decode_sensitive(x), x)
  expect_identical(encode_sensitive("foo234223bar"), "<<my-key>>")
  # FIXME:
  # There's no way to put back the real string unless
  # we stored it somehow, but that seems like an added security risk
  # expect_identical(decode_sensitive(encode_sensitive(x)), x)
})

test_that("decode_sensitive handles zero length raw", {
  local_vcr_configure(
    filter_sensitive_data = list("<<a>>" = "asdfasdf")
  )

  expect_type(the$config$filter_sensitive_data, "list")
  expect_identical(decode_sensitive(raw()), raw())
})
