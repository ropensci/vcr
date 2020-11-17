test_that("serializer_fetch", {
  expect_error(serializer_fetch(), "missing")
  expect_error(serializer_fetch("foo"), "not registered")

  z <- serializer_fetch(name = "foo")
  expect_is(z, "YAML")
  expect_is(z, "Serializer")
  expect_match(z$path, "foo.yml")
  expect_equal(z$file_extension, ".yml")
})
