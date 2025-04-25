test_that("can find matching interations", {
  req1 <- Request$new("GET", "http://a.com")
  req2 <- Request$new("GET", "http://b.com")
  resp1 <- VcrResponse$new(body = "a")
  resp2 <- VcrResponse$new(body = "b")
  interactions <- HTTPInteractionList$new(list(
    list(request = req1, response = resp1),
    list(request = req2, response = resp2)
  ))

  expect_equal(interactions$find_request(req1), 1)
  expect_equal(interactions$find_request(req2), 2)
  expect_equal(interactions$has_interaction(req2), TRUE)
  expect_equal(interactions$has_used_interaction(req1), FALSE)
  expect_equal(interactions$response_for(req2), resp2)
})

test_that("handles non-matches", {
  req1 <- Request$new("GET", "http://a.com")
  req2 <- Request$new("GET", "http://b.com")
  resp1 <- VcrResponse$new(body = "a")
  resp2 <- VcrResponse$new(body = "b")

  interactions <- HTTPInteractionList$new(list(
    list(request = req1, response = resp1),
    list(request = req2, response = resp2)
  ))
  req3 <- Request$new("GET", "http://c.com")

  expect_false(interactions$has_interaction(req3))
  expect_false(interactions$has_used_interaction(req3))
  expect_equal(interactions$find_request(req3), NA_integer_)
  expect_equal(interactions$response_for(req3), NULL)
})

test_that("response_for marks as used", {
  req1 <- Request$new("GET", "http://a.com")
  req2 <- Request$new("GET", "http://b.com")
  resp1 <- VcrResponse$new(body = "a")
  resp2 <- VcrResponse$new(body = "b")
  interactions <- HTTPInteractionList$new(list(
    list(request = req1, response = resp1),
    list(request = req2, response = resp2)
  ))

  expect_equal(interactions$used, c(FALSE, FALSE))
  expect_equal(interactions$remaining_unused_interaction_count(), 2)
  expect_false(interactions$has_used_interaction(req2))

  interactions$response_for(req2)
  expect_equal(interactions$used, c(FALSE, TRUE))
  expect_equal(interactions$remaining_unused_interaction_count(), 1)
  expect_true(interactions$has_used_interaction(req2))
  expect_equal(interactions$response_for(req2), NULL)
})

test_that("can optionally replay", {
  req1 <- Request$new("GET", "http://a.com")
  req2 <- Request$new("GET", "http://b.com")
  resp1 <- VcrResponse$new(body = "a")
  resp2 <- VcrResponse$new(body = "b")
  interactions <- HTTPInteractionList$new(
    list(
      list(request = req1, response = resp1),
      list(request = req2, response = resp2)
    ),
    allow_playback_repeats = TRUE
  )

  interactions$response_for(req2)
  expect_equal(interactions$response_for(req2), resp2)
})
