test_that("checks constructor args", {
  expect_snapshot(error = TRUE, {
    Cassette$new()
    Cassette$new("test", record = "stuff")
    Cassette$new("test", match_requests_on = "x")
    Cassette$new("test", serialize_with = "howdy")
    Cassette$new("test", preserve_exact_body_bytes = 5)
  })
})

test_that("cassette warns if ejected with no interactions", {
  cl <- Cassette$new("test")
  cl$insert()
  expect_snapshot(. <- cl$eject())
})

test_that("make_http_interaction works as expected", {
  local_vcr_configure(dir = withr::local_tempdir())
  #### Prepare http responses
  # crul_resp1 <- crul::HttpClient$new(hb("/get?foo=bar"))$get()
  # save(crul_resp1, file = "tests/testthat/crul_resp1.rda", version = 2)

  # crul_resp2 <- crul::HttpClient$new(hb("/image/png"))$get()
  # save(crul_resp2, file = "tests/testthat/crul_resp2.rda", version = 2)

  # httr_resp1 <- httr::GET(hb("/get?foo=bar"))
  # save(httr_resp1, file = "tests/testthat/httr_resp1.rda", version = 2)

  # httr_resp2 <- httr::GET(hb("/image/png"))
  # save(httr_resp2, file = "tests/testthat/httr_resp2.rda", version = 2)

  zz <- Cassette$new("test")

  # crul, with non-image response body
  # $response$body should be class `character`
  load("crul_resp1.rda")
  aa <- zz$make_http_interaction(crul_resp1)
  expect_s3_class(aa, "HTTPInteraction")
  expect_s3_class(aa$request, "Request")
  expect_s3_class(aa$response, "VcrResponse")
  expect_type(aa$response$body, "character")

  # crul, with image response body
  # $response$body should be class `raw`
  load("crul_resp2.rda")
  bb <- zz$make_http_interaction(crul_resp2)
  expect_s3_class(bb, "HTTPInteraction")
  expect_s3_class(bb$request, "Request")
  expect_s3_class(bb$response, "VcrResponse")
  expect_type(bb$response$body, "raw")
})

test_that("cassette inherit options from vcr_configuration()", {
  local_vcr_configure(
    record = "none",
    match_requests_on = "body",
    serialize_with = "json",
    re_record_interval = 1,
    preserve_exact_body_bytes = TRUE,
    clean_outdated_http_interactions = TRUE
  )
  cas1 <- Cassette$new("test")
  expect_equal(cas1$record, "none")
  expect_equal(cas1$match_requests_on, "body")
  expect_equal(cas1$serialize_with, "json")
  expect_equal(cas1$re_record_interval, 1)
  expect_true(cas1$preserve_exact_body_bytes)
  expect_true(cas1$clean_outdated_http_interactions)

  # But can override as needed
  cas2 <- Cassette$new(
    "test",
    record = "new_episodes",
    match_requests_on = "query",
    preserve_exact_body_bytes = FALSE
  )
  expect_equal(cas2$record, "new_episodes")
  expect_equal(cas2$match_requests_on, "query")
  expect_false(cas2$preserve_exact_body_bytes)
})
