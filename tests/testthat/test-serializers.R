test_that("serializer_fetch", {
  expect_error(serializer_fetch(), "missing")
  expect_error(serializer_fetch("foo"), "not registered")

  z <- serializer_fetch(name = "foo")
  expect_is(z, "YAML")
  expect_is(z, "Serializer")
  expect_match(z$path, "foo.yml")
  expect_equal(z$file_extension, ".yml")
})


tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("you can record a new cassette of same name with different serializer", {
  library(crul)
  mydir <- file.path(tempdir(), "asdfasdfsd")
  invisible(vcr_configure(dir = mydir))
  unlink(file.path(vcr_c$dir, "testing1.yml"))
  cas_yml <- use_cassette(name = "testing1", {
    res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  }, serialize_with = "yaml")
  cas_json <- use_cassette(name = "testing1", {
    res2 <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  }, serialize_with = "json")
  expect_is(cas_yml, "Cassette")
  expect_is(cas_json, "Cassette")
  expect_equal(cas_yml$name, cas_json$name)
  expect_match(cas_yml$manfile, ".yml")
  expect_match(cas_json$manfile, ".json")
})

