setup({
  vcr_test_configuration()
})

# parameters shared by config and cassettes
# (commented params are not exposed by Cassette$cassette_opts())
params <- c(
  "record",
  "match_requests_on",
  # "re_record_interval",
  # "clean_outdated_http_interactions",
  # "allow_unused_http_interactions",
  "serialize_with",
  "persist_with",
  "preserve_exact_body_bytes"
)

test_that("default cassette options match default config", {
  on.exit({
    unlink(vcr_files())
  })

  vcr_configure(
    warn_on_empty_cassette = FALSE
  )

  config <- VCRConfig$new()
  cas1 <- sw(use_cassette("default-use", {}))

  expect_identical(
    config$as_list()[params],
    cas1$cassette_opts[params]
  )

  cas2 <- insert_cassette("default-insert")
  eject_cassette()

  expect_identical(
    config$as_list()[params],
    cas2$cassette_opts[params]
  )
})

test_that("cassettes inherit configured options", {
  on.exit({
    unlink(vcr_files())
    vcr_test_configuration()
  })

  vcr_configure(
    record = "none",
    match_requests_on = "body",
    preserve_exact_body_bytes = TRUE,
    warn_on_empty_cassette = FALSE
  )

  cas1 <- sw(use_cassette("configured-use", {}))

  expect_match(cas1$record, "none")
  expect_setequal(cas1$match_requests_on, "body")
  expect_true(cas1$preserve_exact_body_bytes)

  cas2 <- insert_cassette("configured-insert")
  eject_cassette()

  expect_match(cas2$record, "none")
  expect_setequal(cas2$match_requests_on, "body")
  expect_true(cas2$preserve_exact_body_bytes)
})

test_that("cassettes can override configured options", {
  on.exit({
    unlink(vcr_files())
    vcr_test_configuration()
  })

  vcr_configure(
    record = "none",
    match_requests_on = "body",
    preserve_exact_body_bytes = TRUE,
    warn_on_empty_cassette = FALSE
  )

  cas1 <- sw(use_cassette("overridden-use", {},
    record = "new_episodes",
    match_requests_on = "query",
    preserve_exact_body_bytes = FALSE
  ))

  expect_match(cas1$record, "new_episodes")
  expect_setequal(cas1$match_requests_on, "query")
  expect_false(cas1$preserve_exact_body_bytes)

  cas2 <- insert_cassette("overridden-insert",
    record = "new_episodes",
    match_requests_on = "query",
    preserve_exact_body_bytes = FALSE
  )
  eject_cassette()

  expect_match(cas2$record, "new_episodes")
  expect_setequal(cas2$match_requests_on, "query")
  expect_false(cas2$preserve_exact_body_bytes)
})
