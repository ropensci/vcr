context("RequestMatcherRegistry")

test_that("RequestMatcherRegistry contains the right stuff", {
  expect_is(request_matchers, "RequestMatcherRegistry")
  expect_is(request_matchers$default_matchers, "list")
  expect_equal(request_matchers$default_matchers, list('method', 'uri'))
  expect_is(request_matchers$register, "function")
  expect_is(request_matchers$register_built_ins, "function")
  expect_is(request_matchers$registry, "list")
  expect_is(request_matchers$registry[[1]], "Matcher")
  expect_is(request_matchers$try_to_register_body_as_json, "function")
})

test_that("RequestMatcherRegistry basic functionality works", {
  one <- list(method = "get", uri = "http://foo.bar",
              body = '{"key": "value"}', headers = list(a = 5, b = 6))
  two <- list(method = "post", uri = "http://foo.bar/world",
              body = '{"key": "value"}', headers = list(a = 5, c = 7))
  three <- list(method = "get", uri = "http://foo.bar?stuff=things",
              body = '{"key": "another-value"}', headers = list(a = 5, c = 7))
  four <- list(method = "post", uri = "http://foo.bar?stuff=things7",
                body = '{"key": "another-value"}', headers = list(a = 5, c = 7))
  five <- list(method = "post", uri = "http://foo.bar/world?stuff=things7",
               body = '{"key": "another-value"}', headers = list(a = 5, c = 7))
  a <- Request$new()$from_hash(one)
  b <- Request$new()$from_hash(two)
  c <- Request$new()$from_hash(three)
  d <- Request$new()$from_hash(four)
  e <- Request$new()$from_hash(five)

  # method
  expect_false(request_matchers$registry$method$matches(a, b))
  expect_true(request_matchers$registry$method$matches(a, c))
  expect_false(request_matchers$registry$method$matches(a, d))
  expect_false(request_matchers$registry$method$matches(b, c))
  expect_true(request_matchers$registry$method$matches(b, d))

  # uri
  expect_false(request_matchers$registry$uri$matches(a, b))
  expect_false(request_matchers$registry$uri$matches(a, c))
  expect_false(request_matchers$registry$uri$matches(a, d))
  expect_false(request_matchers$registry$uri$matches(b, c))
  expect_false(request_matchers$registry$uri$matches(b, d))
  expect_false(request_matchers$registry$uri$matches(c, d))

  # body
  expect_true(request_matchers$registry$body$matches(a, b))
  expect_false(request_matchers$registry$body$matches(a, c))
  expect_false(request_matchers$registry$body$matches(a, d))
  expect_false(request_matchers$registry$body$matches(b, c))
  expect_false(request_matchers$registry$body$matches(b, d))
  expect_true(request_matchers$registry$body$matches(c, d))

  # headers
  expect_false(request_matchers$registry$headers$matches(a, b))
  expect_false(request_matchers$registry$headers$matches(a, c))
  expect_false(request_matchers$registry$headers$matches(a, d))
  expect_true(request_matchers$registry$headers$matches(b, c))
  expect_true(request_matchers$registry$headers$matches(b, d))
  expect_true(request_matchers$registry$headers$matches(c, d))

  # host
  expect_true(request_matchers$registry$host$matches(a, b))
  expect_true(request_matchers$registry$host$matches(a, c))
  expect_true(request_matchers$registry$host$matches(a, d))
  expect_true(request_matchers$registry$host$matches(b, c))
  expect_true(request_matchers$registry$host$matches(b, d))
  expect_true(request_matchers$registry$host$matches(c, d))

  # path
  expect_false(request_matchers$registry$path$matches(a, b))
  expect_true(request_matchers$registry$path$matches(a, c))
  expect_true(request_matchers$registry$path$matches(a, d))
  expect_false(request_matchers$registry$path$matches(b, c))
  expect_false(request_matchers$registry$path$matches(b, d))
  expect_true(request_matchers$registry$path$matches(c, d))
  expect_true(request_matchers$registry$path$matches(b, e))

  # query
  expect_true(request_matchers$registry$body_as_json$matches(a, b))
  expect_false(request_matchers$registry$body_as_json$matches(a, c))
  expect_false(request_matchers$registry$body_as_json$matches(a, d))
  expect_false(request_matchers$registry$body_as_json$matches(b, c))
  expect_false(request_matchers$registry$body_as_json$matches(b, d))
  expect_true(request_matchers$registry$body_as_json$matches(c, d))
  expect_false(request_matchers$registry$body_as_json$matches(b, e))
})
