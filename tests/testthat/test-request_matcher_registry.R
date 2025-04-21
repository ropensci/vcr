test_that("can match by method", {
  expect_true(request_matches_one(
    "method",
    list(method = "GET"),
    list(method = "GET")
  ))
  expect_false(request_matches_one(
    "method",
    list(method = "GET"),
    list(method = "POST")
  ))
})

test_that("can match by url", {
  expect_true(request_matches_one(
    "uri",
    list(uri = "http://a.com"),
    list(uri = "http://a.com")
  ))
  expect_false(request_matches_one(
    "uri",
    list(uri = "http://a.com"),
    list(uri = "http://b.com")
  ))
})

test_that("can match by body", {
  expect_true(request_matches_one(
    "body",
    list(body = charToRaw("abc")),
    list(body = charToRaw("abc"))
  ))
  expect_false(request_matches_one(
    "body",
    list(body = charToRaw("abc")),
    list(body = charToRaw("def"))
  ))

  expect_true(request_matches_one(
    "body",
    list(body = "abc"),
    list(body = "abc")
  ))
  expect_false(request_matches_one(
    "body",
    list(body = "abc"),
    list(body = "def")
  ))
})
