test_that("serializer_fetch", {
  z <- Serializer$new()
  expect_s3_class(z, "Serializer")
  # by default assigns a path in a temp dir
  expect_type(z$path, "character")
  expect_false(file.exists(z$path))
  # by default file_extension is NULL
  expect_null(z$file_extension)
  # methods
  expect_type(z$serialize, "closure")
  expect_type(z$deserialize, "closure")
  # method bodies are empty as they're overwritten by children
  expect_equal(as.character(functionBody(z$serialize)), "{")
  expect_equal(as.character(functionBody(z$deserialize)), "{")
  # can set path
  w <- Serializer$new(path = "foobar")
  expect_null(w$file_extension)
  expect_equal(basename(w$path), "foobar")
})

test_that("serializer_fetch", {
  z <- serializer_fetch("foo", "yaml")
  expect_s3_class(z, "YAML")
})

test_that("useful error if not registered", {
  expect_snapshot(serializer_fetch(name = "foo"), error = TRUE)
})

test_that("you can record a new cassette of same name with different serializer", {
  local_vcr_configure(dir = withr::local_tempdir())

  cas_yml <- use_cassette(
    name = "testing1",
    res <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "yaml"
  )
  expect_match(cas_yml$manfile, ".yml")
  cas_json <- use_cassette(
    name = "testing1",
    res2 <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "json"
  )
  expect_match(cas_json$manfile, ".json")

  expect_equal(cas_yml$name, cas_json$name)
})
