context("HTTPInteractionList")

test_that("HTTPInteractionList", {
  crul::mock(FALSE)

  url <- "https://eu.httpbin.org/post"
  body <- list(foo = "bar")
  cli <- crul::HttpClient$new(url = url)
  res <- cli$post(body = body)

  # request
  request <- Request$new("POST", uri = url,
    body = body, headers = res$response_headers)
  # response
  response <- VcrResponse$new(
     res$status_http(),
     res$response_headers,
     res$parse("UTF-8"),
     res$response_headers$status)

  # make HTTPInteraction object
  inter <- HTTPInteraction$new(request = request, response = response)

  # make HTTPInteractionList object
  x <- suppressMessages(HTTPInteractionList$new(
     interactions = list(inter),
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
})
