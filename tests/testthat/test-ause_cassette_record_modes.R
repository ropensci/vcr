conn <- crul::HttpClient$new(hb())

test_that("use_cassette record mode: once", {
  # record mode `once`:
  # - Replay previously recorded interactions.
  # - Record new interactions if there is no cassette file.
  # - Cause an error to be raised for new requests if there is a cassette file.
  local_vcr_configure(dir = withr::local_tempdir())

  # record interaction
  one <- use_cassette(
    "once",
    res <- conn$get("get"),
    record = "once"
  )
  expect_s3_class(one, "Cassette")
  expect_s3_class(res, "HttpResponse")

  # interaction should replay
  #  - we know it replayed if it doesn't timeout as timeout only
  #   used in real request
  two <- use_cassette(
    "once",
    res2 <- conn$get("get", timeout_ms = 10),
    record = "once"
  )
  expect_s3_class(two, "Cassette")

  # check recorded interactions
  expect_equal(sum(one$http_interactions$used), 0)
  expect_equal(sum(two$http_interactions$used), 1)
  expect_equal(length(one$new_recorded_interactions), 1)
  expect_equal(length(two$new_recorded_interactions), 0)
  expect_identical(res$content, res2$content)

  # delete cassette file, new interaction should be recorded successfully
  unlink(file.path(vcr_c$dir, "once.yml"))
  expect_false(file.exists(file.path(vcr_c$dir, "once.yml")))
  three <- use_cassette(
    "once",
    res3 <- conn$get("get"),
    record = "once"
  )
  expect_s3_class(three, "Cassette")

  # raise error on attempted NEW INTERACTION on existing cassette file
  expect_error(
    use_cassette(
      "once",
      conn$get("get", query = list(foo = "bar")),
      record = "once"
    ),
    "An HTTP request has been made that vcr does not know how to handle"
  )
})

test_that("use_cassette record mode: none", {
  # record mode `none`:
  # - Replay previously recorded interactions.
  # - Cause an error to be raised for any new requests.
  local_vcr_configure(dir = withr::local_tempdir())

  # record first with another record mode to make the cassette
  invisible(use_cassette(
    "none",
    res <- conn$get("get")
  ))

  # previously recorded interaction should replay
  one <- use_cassette(
    "none",
    res <- conn$get("get"),
    record = "none"
  )
  expect_s3_class(one, "Cassette")
  expect_s3_class(res, "HttpResponse")

  # raise error if any NEW INTERACTIONS attempted
  # FIXME:
  expect_error(
    use_cassette(
      "none",
      conn$get("get", query = list(foo = "bar")),
      record = "none"
    ),
    "vcr does not know how to handle"
    # "The current record mode \\('none'\\) does not"
  )
})

test_that("use_cassette record mode: new_episodes", {
  # record mode `new_episodes`:
  # - Record new interactions.
  # - Replay previously recorded interactions.
  local_vcr_configure(dir = withr::local_tempdir())

  # record first interaction
  one <- use_cassette(
    "new_episodes",
    res <- conn$get("get"),
    record = "new_episodes"
  )
  expect_s3_class(one, "Cassette")
  expect_s3_class(res, "HttpResponse")
  one_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(one_yml$http_interactions), 1)

  # first interaction again, should be played back
  one_again <- use_cassette(
    "new_episodes",
    res2 <- conn$get("get"),
    record = "new_episodes"
  )
  expect_s3_class(one_again, "Cassette")
  expect_s3_class(res2, "HttpResponse")
  one_again_yml <- yaml::yaml.load_file(file.path(
    vcr_c$dir,
    "new_episodes.yml"
  ))
  expect_equal(length(one_again_yml$http_interactions), 1)

  # record new interaction, is recorded below first one above
  two <- use_cassette(
    "new_episodes",
    res3 <- conn$get("get", query = list(project = "mars-explorer")),
    record = "new_episodes"
  )
  expect_s3_class(two, "Cassette")
  expect_s3_class(res3, "HttpResponse")
  two_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(two_yml$http_interactions), 2)

  # first and second interaction again together, both should be played back
  yolo <- use_cassette(
    "new_episodes",
    {
      res2_played_back <- conn$get("get")
      res3_played_back <- conn$get(
        "get",
        query = list(project = "mars-explorer")
      )
    },
    record = "new_episodes"
  )
  expect_s3_class(two, "Cassette")
  expect_s3_class(res3, "HttpResponse")
  two_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(two_yml$http_interactions), 2)
})

test_that("use_cassette record mode: all", {
  # record mode `all`:
  # - Record new interactions.
  # - Never replay previously recorded interactions.
  local_vcr_configure(dir = withr::local_tempdir())

  # record first interaction
  all_1 <- use_cassette("all", conn$get("get"), record = "all")
  interactions_1 <- all_1$merged_interactions()
  expect_length(interactions_1, 1)

  # sleep for a bit to make sure times are at least a second apart
  Sys.sleep(1)

  # previously recorded interactions do not playback
  # - recorded time and response header time have changed
  all_2 <- use_cassette("all", conn$get("get"), record = "all")
  interactions_2 <- all_2$merged_interactions()
  expect_length(interactions_2, 1)

  ## recorded time is updated
  expect_lt(
    interactions_1[[1]]$recorded_at,
    interactions_2[[1]]$recorded_at
  )
  # and request is re-perf
  expect_lt(
    parse_http_date(interactions_1[[1]]$response$headers$date),
    parse_http_date(interactions_2[[1]]$response$headers$date)
  )

  # new interactions are recorded
  three <- use_cassette(
    "all",
    res3 <- conn$get("get", query = list(cheese = "pepperjack")),
    record = "all"
  )
  three_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "all.yml"))
  expect_equal(length(three_yml$http_interactions), 2)
  expect_match(
    three_yml$http_interactions[[2]]$response$body$string,
    "pepperjack"
  )
})
