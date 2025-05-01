test_that("checks constructor args", {
  expect_snapshot(error = TRUE, {
    Cassette$new()
    Cassette$new("test", record = "stuff")
    Cassette$new("test", match_requests_on = "x")
    Cassette$new("test", serialize_with = "howdy")
    Cassette$new("test", preserve_exact_body_bytes = 5)
  })
})

test_that("has nice print method", {
  expect_snapshot(Cassette$new("test"))
})

test_that("cassette warns if ejected with no interactions", {
  cl <- Cassette$new("test")
  cl$insert()
  expect_snapshot(. <- cl$eject())
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

test_that("cassette checks name", {
  expect_snapshot(error = TRUE, {
    Cassette$new("foo bar")
    Cassette$new("foo.yml")
    Cassette$new("foo/bar")
    Cassette$new("foo\nbar")
    Cassette$new("foo\nbar.")
    Cassette$new("..")
    Cassette$new("con")
    Cassette$new(strrep("x", 400))
  })

  local_vcr_configure(warn_on_empty_cassette = FALSE)
  local_cassette("foo")
  expect_snapshot(Cassette$new("foo"), error = TRUE)
})

test_that("important interactions are logged", {
  dir <- withr::local_tempdir()
  local_vcr_configure(dir = dir)
  local_vcr_configure_log(file = stdout())

  expect_snapshot(
    {
      use_cassette("test", httr::GET(hb("/html")))
      use_cassette("test", httr::GET(hb("/html")))
      use_cassette("test", httr::GET(hb("/404")))
    },
    error = TRUE,
    transform = \(x)
      x |>
        gsub(hb(), "{httpbin}", x = _, fixed = TRUE) |>
        gsub(dir, "{dir}", x = _, fixed = TRUE)
  )
})
