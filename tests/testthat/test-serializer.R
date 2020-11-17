test_that("serializer_fetch", {
  expect_is(Serializer, "R6ClassGenerator")

  z <- Serializer$new()
  expect_is(z, "Serializer")
  # by default assigns a path in a temp dir
  expect_is(z$path, "character")
  expect_false(file.exists(z$path))
  # by default file_extension is NULL
  expect_null(z$file_extension)
  # methods
  expect_is(z$serialize, "function")
  expect_is(z$deserialize, "function")
  # method bodies are empty as they're overwritten by children
  expect_equal(as.character(functionBody(z$serialize)), "{")
  expect_equal(as.character(functionBody(z$deserialize)), "{")
  # can set path
  w <- Serializer$new(path = "foobar")
  expect_null(w$file_extension)
  expect_equal(basename(w$path), "foobar")
})
