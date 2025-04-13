test_that("serializer_fetch", {
  expect_error(serializer_fetch(), "missing")
  expect_error(serializer_fetch("foo"), "not registered")

  z <- serializer_fetch(name = "foo")
  expect_s3_class(z, "YAML")
  expect_s3_class(z, "Serializer")
  expect_match(z$path, "foo.yml")
  expect_equal(z$file_extension, ".yml")
})

test_that("you can record a new cassette of same name with different serializer", {
  local_vcr_configure(dir = withr::local_tempdir())

  cas_yml <- use_cassette(
    name = "testing1",

    {
      res <- crul::HttpClient$new(hb("/get"))$get()
    },

    serialize_with = "yaml"
  )
  cas_json <- use_cassette(
    name = "testing1",

    {
      res2 <- crul::HttpClient$new(hb("/get"))$get()
    },

    serialize_with = "json"
  )
  expect_s3_class(cas_yml, "Cassette")
  expect_s3_class(cas_json, "Cassette")
  expect_equal(cas_yml$name, cas_json$name)
  expect_match(cas_yml$manfile, ".yml")
  expect_match(cas_json$manfile, ".json")
})
