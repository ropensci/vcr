skip_on_cran()

library("crul")
vcr_configure(dir = tempdir())

context("adapter-crul: POST requests works")
test_that("crul POST requests works", {
  # body type: named list
  out <- use_cassette("crul_post_named_list", {
    x <- HttpClient$new("https://httpbin.org/post")$post(body = list(foo = "bar"))
  })
  expect_false(out$is_empty())
  expect_is(x, "HttpResponse")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$form, list(foo = "bar"))

  # body type: character
  out2 <- use_cassette("crul_post_string", {
    z <- HttpClient$new("https://httpbin.org/post")$post(body = "some string")
  })
  expect_false(out2$is_empty())
  expect_is(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: raw
  out3 <- use_cassette("crul_post_raw", {
    z <- HttpClient$new("https://httpbin.org/post")$post(body = charToRaw("some string"))
  })
  expect_false(out3$is_empty())
  expect_is(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: upload_file
  ## upload_file in a list
  ff <- tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette("crul_post_upload_file", {
    b <- HttpClient$new("https://httpbin.org/post")$post(
      body = list(y = crul::upload(ff)))
  })
  expect_false(out4$is_empty())
  expect_is(b, "HttpResponse")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$y, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty
  unlink(ff)
  
  ## upload_file not in a list
  out6 <- use_cassette("crul_post_upload_file_no_list", {
    d <- HttpClient$new("https://httpbin.org/post")$post(
      body = crul::upload(system.file("CITATION")))
  })
  expect_false(out6$is_empty())
  expect_is(d, "HttpResponse")
  expect_equal(d$status_code, 200)
  str <- yaml::yaml.load_file(out6$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(length(strj$files), 0) # files empty
  expect_match(strj$data, "bibentry\\(") # data not empty

  # body type: NULL
  out5 <- use_cassette("crul_post_null", {
    m <- HttpClient$new("https://httpbin.org/post")$post(body = NULL)
  })
  expect_false(out5$is_empty())
  expect_is(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out5$manfile)$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")

  # cleanup
  unlink(file.path(vcr_configuration()$dir, "crul_post_named_list.yml"))
  unlink(file.path(vcr_configuration()$dir, "crul_post_string.yml"))
  unlink(file.path(vcr_configuration()$dir, "crul_post_raw.yml"))
  unlink(file.path(vcr_configuration()$dir, "crul_post_upload_file.yml"))
  unlink(file.path(vcr_configuration()$dir, "crul_post_null.yml"))
  unlink(file.path(vcr_configuration()$dir, "crul_post_upload_file_no_list.yml"))
})
