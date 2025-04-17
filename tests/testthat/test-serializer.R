test_that("serializer_fetch", {
  z <- Serializer$new("path", "name", ".ext")
  expect_s3_class(z, "Serializer")
  expect_equal(z$path, "path/name.ext")
  expect_equal(z$file_extension, ".ext")
})

test_that("serializer_fetch", {
  z <- serializer_fetch("yaml", "path", "name")
  expect_s3_class(z, "YAML")
})

test_that("useful error if not registered", {
  expect_snapshot(serializer_fetch("foo"), error = TRUE)
})

test_that("you can record a new cassette of same name with different serializer", {
  local_vcr_configure(dir = withr::local_tempdir())

  cas_yml <- use_cassette(
    name = "testing1",
    res <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "yaml"
  )
  expect_match(cas_yml$file(), ".yml")
  cas_json <- use_cassette(
    name = "testing1",
    res2 <- crul::HttpClient$new(hb("/get"))$get(),
    serialize_with = "json"
  )
  expect_match(cas_json$file(), ".json")

  expect_equal(cas_yml$name, cas_json$name)
})
