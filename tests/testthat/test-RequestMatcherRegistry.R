test_that("RequestMatcherRegistry basic functionality works", {
  a <- Request$new(
    method = "get",
    uri = "http://foo.bar",
    body = '{"key": "value"}',
    headers = list(a = 5, b = 6)
  )
  b <- Request$new(
    method = "post",
    uri = "http://foo.bar/world",
    body = '{"key": "value"}',
    headers = list(a = 5, c = 7)
  )
  c <- Request$new(
    method = "get",
    uri = "http://foo.bar?stuff=things",
    body = '{"key": "another-value"}',
    headers = list(a = 5, c = 7)
  )
  d <- Request$new(
    method = "post",
    uri = "http://foo.bar?stuff=things7",
    body = '{"key": "another-value"}',
    headers = list(a = 5, c = 7)
  )
  e <- Request$new(
    method = "post",
    uri = "http://foo.bar/world?stuff=things7",
    body = '{"key": "another-value"}',
    headers = list(a = 5, c = 7)
  )

  # method
  rm <- RequestMatcherRegistry$new()
  expect_false(rm$registry$method$matches(a, b))
  expect_true(rm$registry$method$matches(a, c))
  expect_false(rm$registry$method$matches(a, d))
  expect_false(rm$registry$method$matches(b, c))
  expect_true(rm$registry$method$matches(b, d))

  # uri
  expect_false(rm$registry$uri$matches(a, b))
  expect_false(rm$registry$uri$matches(a, c))
  expect_false(rm$registry$uri$matches(a, d))
  expect_false(rm$registry$uri$matches(b, c))
  expect_false(rm$registry$uri$matches(b, d))
  expect_false(rm$registry$uri$matches(c, d))

  # body
  expect_true(rm$registry$body$matches(a, b))
  expect_false(rm$registry$body$matches(a, c))
  expect_false(rm$registry$body$matches(a, d))
  expect_false(rm$registry$body$matches(b, c))
  expect_false(rm$registry$body$matches(b, d))
  expect_true(rm$registry$body$matches(c, d))

  # headers
  expect_false(rm$registry$headers$matches(a, b))
  expect_false(rm$registry$headers$matches(a, c))
  expect_false(rm$registry$headers$matches(a, d))
  expect_true(rm$registry$headers$matches(b, c))
  expect_true(rm$registry$headers$matches(b, d))
  expect_true(rm$registry$headers$matches(c, d))

  # host
  expect_true(rm$registry$host$matches(a, b))
  expect_true(rm$registry$host$matches(a, c))
  expect_true(rm$registry$host$matches(a, d))
  expect_true(rm$registry$host$matches(b, c))
  expect_true(rm$registry$host$matches(b, d))
  expect_true(rm$registry$host$matches(c, d))

  # path
  expect_false(rm$registry$path$matches(a, b))
  expect_true(rm$registry$path$matches(a, c))
  expect_true(rm$registry$path$matches(a, d))
  expect_false(rm$registry$path$matches(b, c))
  expect_false(rm$registry$path$matches(b, d))
  expect_true(rm$registry$path$matches(c, d))
  expect_true(rm$registry$path$matches(b, e))
  ## trailing slash is removed
  expect_true(rm$registry$path$matches(
    list(path = "foo"),
    list(path = "foo/")
  ))

  # query
  expect_true(rm$registry$body_as_json$matches(a, b))
  expect_false(rm$registry$body_as_json$matches(a, c))
  expect_false(rm$registry$body_as_json$matches(a, d))
  expect_false(rm$registry$body_as_json$matches(b, c))
  expect_false(rm$registry$body_as_json$matches(b, d))
  expect_true(rm$registry$body_as_json$matches(c, d))
  expect_false(rm$registry$body_as_json$matches(b, e))
})
