context("use_cassette: record modes work as expected")

library(crul, quietly = TRUE)
mydir <- file.path(tempdir(), "use_cassette_record_mode")
invisible(vcr_configure(dir = mydir))
conn <- crul::HttpClient$new("https://eu.httpbin.org")

test_that("use_cassette record mode: once", {
  # record mode `once`:
  # - Replay previously recorded interactions.
  # - Record new interactions if there is no cassette file.
  # - Cause an error to be raised for new requests if there is a cassette file.
  skip_on_cran()

  unlink(file.path(vcr_c$dir, "once.yml"))

  # record interaction
  one <- use_cassette("once", (res <- conn$get("get")), record = "once")
  expect_is(one, "Cassette")
  expect_is(res, "HttpResponse")

  # interaction should replay
  #  - we know it replayed if it doesn't timeout as timeout only
  #   used in real request
  two <- use_cassette("once", {
    res2 <- conn$get("get", timeout_ms = 10)
  }, record = "once")
  expect_is(two, "Cassette")
  expect_equal(length(one$http_interactions_$used_interactions), 0)
  expect_equal(length(two$http_interactions_$used_interactions), 1)
  expect_equal(length(one$new_recorded_interactions), 1)
  expect_equal(length(two$new_recorded_interactions), 0)
  expect_identical(res$content, res2$content)

  # delete cassette file, new interaction should be recorded successfully
  unlink(file.path(vcr_c$dir, "once.yml"))
  expect_false(file.exists(file.path(vcr_c$dir, "once.yml")))
  three <- use_cassette("once", (res3 <- conn$get("get")),
    record = "once")
  expect_is(three, "Cassette")

  # raise error on attempted NEW INTERACTION on existing cassette file
  expect_error(
    use_cassette("once", conn$get("get", query = list(foo = "bar")),
      record = "once"),
    "An HTTP request has been made that vcr does not know how to handle"
  )
})

test_that("use_cassette record mode: none", {
  # record mode `none`:
  # - Replay previously recorded interactions.
  # - Cause an error to be raised for any new requests.
  skip_on_cran()

  unlink(file.path(vcr_c$dir, "none.yml"))

  # record first with another record mode to make the cassette
  invisible(use_cassette("none", (res <- conn$get("get"))))

  # previously recorded interaction should replay
  one <- use_cassette("none", (res <- conn$get("get")), record = "none")
  expect_is(one, "Cassette")
  expect_is(res, "HttpResponse")

  # raise error if any NEW INTERACTIONS attempted
  expect_error(
    use_cassette("none", conn$get("get", query = list(foo = "bar")),
      record = "none"),
    "The current record mode \\('none'\\) does not"
  )
})

test_that("use_cassette record mode: new_episodes", {
  # record mode `new_episodes`:
  # - Record new interactions.
  # - Replay previously recorded interactions.
  skip_on_cran()

  unlink(file.path(vcr_c$dir, "new_episodes.yml"))

  # record first interaction
  one <- use_cassette("new_episodes", {
    res <- conn$get("get")
  }, record = "new_episodes")
  expect_is(one, "Cassette")
  expect_is(res, "HttpResponse")
  one_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(one_yml$http_interactions), 1)

  # first interaction again, should be played back
  one_again <- use_cassette("new_episodes", {
    res2 <- conn$get("get")
  }, record = "new_episodes")
  expect_is(one_again, "Cassette")
  expect_is(res2, "HttpResponse")
  one_again_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(one_again_yml$http_interactions), 1)

  # record new interaction, is recorded below first one above
  two <- use_cassette("new_episodes", {
    res3 <- conn$get("get", query = list(project = "mars-explorer"))
  }, record = "new_episodes")
  expect_is(two, "Cassette")
  expect_is(res3, "HttpResponse")
  two_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(two_yml$http_interactions), 2)

  # first and second interaction again together, both should be played back
  yolo <- use_cassette("new_episodes", {
    res2_played_back <- conn$get("get")
    res3_played_back <- conn$get("get", query = list(project = "mars-explorer"))
  }, record = "new_episodes")
  expect_is(two, "Cassette")
  expect_is(res3, "HttpResponse")
  two_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "new_episodes.yml"))
  expect_equal(length(two_yml$http_interactions), 2)
})

test_that("use_cassette record mode: all", {
  # record mode `all`:
  # - Record new interactions.
  # - Never replay previously recorded interactions.
  skip_on_cran()

  unlink(file.path(vcr_c$dir, "all.yml"))

  # record first interaction
  one <- use_cassette("all", (res <- conn$get("get")), record = "all")
  one_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "all.yml"))
  expect_equal(length(one_yml$http_interactions), 1)

  # sleep for a bit to make sure times are at least a second apart
  Sys.sleep(1)

  # previously recorded interactions do not playback
  # - recorded time and response header time have changed
  two <- use_cassette("all", (res2 <- conn$get("get")), record = "all")
  two_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "all.yml"))
  expect_equal(length(two_yml$http_interactions), 1)

  ### recorded_at
  expect_false(
    identical(one_yml$http_interactions[[1]]$recorded_at,
      two_yml$http_interactions[[1]]$recorded_at
    )
  )
  expect_true(
    two_yml$http_interactions[[1]]$recorded_at >
    one_yml$http_interactions[[1]]$recorded_at
  )
  ### response headers date
  expect_false(
    identical(one_yml$http_interactions[[1]]$response$headers$date,
      two_yml$http_interactions[[1]]$response$headers$date
    )
  )
  expect_true(
    two_yml$http_interactions[[1]]$response$headers$date >
    one_yml$http_interactions[[1]]$response$headers$date
  )

  # new interactions are recorded
  three <- use_cassette("all", {
    res3 <- conn$get("get", query = list(cheese = "pepperjack"))
  }, record = "all")
  three_yml <- yaml::yaml.load_file(file.path(vcr_c$dir, "all.yml"))
  expect_equal(length(three_yml$http_interactions), 2)
  expect_match(three_yml$http_interactions[[2]]$response$body$string,
    "pepperjack")
})

# cleanup
unlink(file.path(vcr_c$dir, "once.yml"))
unlink(file.path(vcr_c$dir, "none.yml"))
unlink(file.path(vcr_c$dir, "new_episodes.yml"))
unlink(file.path(vcr_c$dir, "all.yml"))
# reset configuration
vcr_configure_reset()
