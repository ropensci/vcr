context("json serializer")
test_that("use_cassette with json serializer", {
  skip_on_cran()

  # library(crul)
  # mydir <- file.path(tempdir(), "asdfasdfsd")
  # invisible(vcr_configure(dir = mydir, serialize_with = 'json'))
  # unlink(file.path(vcr_c$dir, "testing1.json"))
  # vcr_configuration()

  # does one request work?
  # aa <- use_cassette("testing2", {
  #   res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  # })

  # does two requests work?
  # cc <- use_cassette("testing4", {
  #   res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  #   the <- crul::HttpClient$new("https://eu.httpbin.org/post")$post(body = "fafaa")
  # }, record = "new_episodes")

  # aa <- use_cassette("testing1", {
  #   res <- crul::HttpClient$new("https://eu.httpbin.org/get")$get()
  # }, serialize_with = "json")

  # x <- insert_cassette("testing1", serialize_with = "json")
})
