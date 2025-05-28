test_that("can find matching interations", {
  req1 <- vcr_request("GET", "http://a.com")
  req2 <- vcr_request("GET", "http://b.com")
  resp1 <- vcr_response(200, body = "a")
  resp2 <- vcr_response(200, body = "b")
  interactions <- Interactions$new(list(
    vcr_interaction(req1, resp1),
    vcr_interaction(req2, resp2)
  ))

  expect_equal(interactions$find_request(req1), 1)
  expect_equal(vcr_last_request(), encode_request(req1))
  expect_equal(vcr_last_response(), encode_response(resp1))

  expect_equal(interactions$find_request(req2), 2)
  expect_equal(vcr_last_request(), encode_request(req2))
  expect_equal(vcr_last_response(), encode_response(resp2))
})

test_that("handles non-matches", {
  req1 <- vcr_request("GET", "http://a.com")
  req2 <- vcr_request("GET", "http://b.com")
  resp1 <- vcr_response(200, body = "a")
  resp2 <- vcr_response(200, body = "b")

  interactions <- Interactions$new(list(
    vcr_interaction(req1, resp1),
    vcr_interaction(req2, resp2)
  ))
  req3 <- vcr_request("GET", "http://c.com")

  expect_equal(interactions$find_request(req3), NA_integer_)
  expect_equal(vcr_last_request(), encode_request(req3))
  expect_equal(vcr_last_response(), NULL)
})

test_that("replay_request marks as used", {
  req1 <- vcr_request("GET", "http://a.com")
  req2 <- vcr_request("GET", "http://b.com")
  resp1 <- vcr_response(200, body = "a")
  resp2 <- vcr_response(200, body = "b")
  interactions <- Interactions$new(list(
    vcr_interaction(req1, resp1),
    vcr_interaction(req2, resp2)
  ))

  expect_equal(interactions$replayable, c(TRUE, TRUE))
  expect_equal(interactions$n_replayable(), 2)
  expect_equal(interactions$find_request(req2), 2)

  interactions$replay_request(2)
  expect_equal(interactions$replayable, c(TRUE, FALSE))
  expect_equal(interactions$n_replayable(), 1)
  expect_equal(interactions$find_request(req2), NA_integer_)
})

test_that("can add interactions", {
  req1 <- vcr_request("GET", "http://a.com")
  resp1 <- vcr_response(200, body = "a")
  resp2 <- vcr_response(200, body = "b")

  interactions <- Interactions$new()

  interactions$add(req1, resp1)
  expect_equal(interactions$interactions[[1]]$request, req1)
  expect_equal(interactions$interactions[[1]]$response, resp1)
  # newly added interactions can not be replayed
  expect_equal(interactions$replayable, FALSE)

  # always add interactions
  interactions$add(req1, resp2)
  expect_equal(interactions$interactions[[1]]$request, req1)
  expect_equal(interactions$interactions[[2]]$response, resp2)
})
