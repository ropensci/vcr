test_that("record mode: once", {
  # record mode `once`:
  # - Replay previously recorded interactions.
  # - Record new interactions if there is no cassette file.
  # - Cause an error to be raised for new requests if there is a cassette file.
  local_vcr_configure(dir = withr::local_tempdir(), record = "once")
  conn <- crul::HttpClient$new(hb())

  # record interaction
  one <- use_cassette("test", res1 <- conn$get("get"))
  expect_equal(one$new_interactions, TRUE)
  expect_equal(one$http_interactions$length(), 1)

  # interaction should replay
  #  - we know it replayed if it doesn't timeout as timeout only
  #   used in real request
  two <- use_cassette("test", res2 <- conn$get("get", timeout_ms = 1))
  expect_equal(two$new_interactions, FALSE)
  expect_equal(two$http_interactions$length(), 1)
  # check recorded interactions
  expect_equal(res2$content, res1$content)

  # delete cassette file, new interaction should be recorded successfully
  unlink(file.path(the$config$dir, "test.yml"))
  three <- use_cassette("test", res3 <- conn$get("get"))
  expect_equal(res3$content, res1$content)

  # raise error on attempted NEW INTERACTION on existing cassette file
  expect_error(
    use_cassette("test", conn$get("get", query = list(foo = "bar"))),
    class = "vcr_unhandled"
  )
})

test_that("record mode: none", {
  # record mode `none`:
  # - Replay previously recorded interactions.
  # - Cause an error to be raised for any new requests.
  local_vcr_configure(dir = withr::local_tempdir(), record = "none")
  conn <- crul::HttpClient$new(hb())

  # record first with another record mode to make the cassette
  use_cassette("test", res1 <- conn$get("get"), record = "once")

  # previously recorded interaction should replay
  one <- use_cassette("test", res2 <- conn$get("get"))
  expect_equal(res2$content, res1$content)

  # raise error if any NEW INTERACTIONS attempted
  expect_error(
    use_cassette("test", conn$get("get", query = list(foo = "bar"))),
    class = "vcr_unhandled",
  )
})

test_that("can replay two requests to the same url", {
  local_vcr_configure(dir = withr::local_tempdir())

  use_cassette("test", {
    res_record <- list(httr::GET(hb("/get")), httr::GET(hb("/get")))
  })
  use_cassette("test", {
    res_replay <- list(httr::GET(hb("/get")), httr::GET(hb("/get")))
  })

  expect_equal(res_record[[1]]$content, res_replay[[1]]$content)
})

test_that("record mode: new_episodes", {
  # record mode `new_episodes`:
  # - Record new interactions.
  # - Replay previously recorded interactions.
  local_vcr_configure(dir = withr::local_tempdir(), record = "new_episodes")
  conn <- crul::HttpClient$new(hb())

  # record first interaction
  one <- use_cassette("test", res1 <- conn$get("get"))
  expect_equal(one$new_interactions, TRUE)
  expect_equal(one$http_interactions$length(), 1)

  # first interaction again, should be played back
  one_replay <- use_cassette("test", res1_replay <- conn$get("get"))
  expect_equal(one_replay$new_interactions, FALSE)
  expect_equal(one_replay$http_interactions$length(), 1)
  expect_equal(res1_replay$content, res1$content)

  # record new interaction, is recorded below first one above
  two <- use_cassette("test", res2 <- conn$get("get", query = list(x = "a")))
  expect_equal(two$new_interactions, TRUE)
  expect_equal(two$http_interactions$length(), 2)

  # first and second interaction again together, both should be played back
  two_replay <- use_cassette("test", {
    res1_replay <- conn$get("get")
    res2_replay <- conn$get("get", query = list(x = "a"))
  })
  expect_equal(two_replay$new_interactions, FALSE)
  expect_equal(two_replay$http_interactions$length(), 2)
  expect_equal(res1_replay$content, res1$content)
  expect_equal(res2_replay$content, res2$content)
})

test_that("record mode: all", {
  # record mode `all`:
  # - Record new interactions.
  # - Never replay previously recorded interactions.
  local_vcr_configure(dir = withr::local_tempdir(), record = "all")
  conn <- crul::HttpClient$new(hb())

  # record first interaction
  one <- use_cassette("test", res1 <- conn$get("get"))
  expect_equal(one$new_interactions, TRUE)
  expect_equal(one$http_interactions$length(), 1)
  interactions_1 <- one$http_interactions$interactions

  # sleep for a bit to make sure times are at least a second apart
  Sys.sleep(1)

  # previously recorded interactions do not playback
  # - recorded time and response header time have changed
  two <- use_cassette("test", res2 <- conn$get("get"))
  expect_equal(two$new_interactions, TRUE)
  expect_equal(two$http_interactions$length(), 1)
  interactions_2 <- two$http_interactions$interactions

  ## recorded time is updated
  expect_lt(interactions_1[[1]]$recorded_at, interactions_2[[1]]$recorded_at)
  # and request is re-perf
  expect_lt(
    httr::parse_http_date(res1$response_headers$date),
    httr::parse_http_date(res2$response_headers$date)
  )

  # new interactions are recorded
  three <- use_cassette("test", res3 <- conn$get("get", query = list(x = "a")))
  expect_equal(three$new_interactions, TRUE)
  expect_equal(three$http_interactions$length(), 1)
})
