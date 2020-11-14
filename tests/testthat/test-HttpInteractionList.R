test_that("HTTPInteractionList", {
  crul::mock(FALSE)

  url <- "https://eu.httpbin.org/post"
  url2 <- "https://eu.httpbin.org/get"
  url3 <- "https://scottchamberlain.info"
  body <- list(foo = "bar")
  cli <- crul::HttpClient$new(url = url)
  res <- cli$post(body = body)
  cli2 <- crul::HttpClient$new(url = url2)
  res2 <- cli2$get(body = body)
  res3 <- crul::HttpClient$new(url = url3)$get()

  # requests
  request <- Request$new("POST", uri = url,
    body = body, headers = res$response_headers)
  request2 <- Request$new("GET", uri = url2,
    body = body, headers = res2$response_headers)
  request3 <- Request$new("GET", uri = url3,
    headers = res3$response_headers)
  # response
  response <- VcrResponse$new(res$status_http(), res$response_headers,
     res$parse("UTF-8"), res$response_headers$status)
  response2 <- VcrResponse$new(res2$status_http(), res2$response_headers,
     res2$parse("UTF-8"), res2$response_headers$status)

  # make HTTPInteraction object
  inter <- HTTPInteraction$new(request = request, response = response)
  inter2 <- HTTPInteraction$new(request = request2, response = response2)

  # make HTTPInteractionList object
  x <- suppressMessages(HTTPInteractionList$new(
     interactions = list(inter, inter2),
     request_matchers = vcr_configuration()$match_requests_on
  ))

  # objects and methods
  expect_is(x$request_matchers, "character")
  expect_equal(x$request_matchers, c('method', 'uri'))
  ## parent list
  expect_is(x$parent_list, "NullList")
  expect_null(x$parent_list$response_for())
  expect_false(x$parent_list$has_interaction_matching())
  expect_false(x$parent_list$has_used_interaction_matching())
  expect_equal(x$parent_list$remaining_unused_interaction_count(), 0)

  expect_is(x$used_interactions, "list")
  expect_false(x$allow_playback_repeats)
  expect_is(x$interactions, "list")
  expect_is(x$interactions[[1]], "HTTPInteraction")
  expect_is(x$response_for, "function")
  expect_is(suppressWarnings(x$response_for(request)), "VcrResponse")

  # private methods
  ### need to remake the HTTPInteractionList object b/c response_for alters it 
  x <- suppressMessages(HTTPInteractionList$new(
     interactions = list(inter, inter2),
     request_matchers = vcr_configuration()$match_requests_on
  ))
  priv <- x$.__enclos_env__$private

  ## has_unused_interactions
  expect_true(priv$has_unused_interactions())
  x$response_for(request) # request used
  x$response_for(request2) # request2 used
  expect_false(priv$has_unused_interactions())

  # remake after response_for use
  x <- suppressMessages(HTTPInteractionList$new(
     interactions = list(inter, inter2),
     request_matchers = vcr_configuration()$match_requests_on
  ))
  priv <- x$.__enclos_env__$private

  ## matching_interaction_bool
  expect_true(priv$matching_interaction_bool(request))
  expect_true(priv$matching_interaction_bool(request2))
  expect_false(priv$matching_interaction_bool(request3))

  ## matching_interaction_index
  expect_equal(priv$matching_interaction_index(request), 1)
  expect_equal(priv$matching_interaction_index(request2), 2)
  expect_length(priv$matching_interaction_index(request3), 0)

  ## matching_used_interaction_for
  ### allow_playback_repeats=FALSE -> returns FALSE no matter what
  expect_false(priv$matching_used_interaction_for(request))

  ### interaction_matches_request
  expect_true(priv$interaction_matches_request(request, x$interactions[[1]]))
  expect_false(priv$interaction_matches_request(request, x$interactions[[2]]))
  expect_false(priv$interaction_matches_request(request2, x$interactions[[1]]))
  expect_true(priv$interaction_matches_request(request2, x$interactions[[2]]))
  
  ### allow_playback_repeats=TRUE -> then we check the used interactions
  x <- suppressMessages(HTTPInteractionList$new(
     interactions = list(inter, inter2),
     request_matchers = vcr_configuration()$match_requests_on,
     allow_playback_repeats = TRUE
  ))
  priv <- x$.__enclos_env__$private
  expect_false(priv$matching_used_interaction_for(request))
  x$response_for(request) # request used
  expect_is(priv$matching_used_interaction_for(request),
    "HTTPInteraction")
})
