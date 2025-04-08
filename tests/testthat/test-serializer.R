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
