skip_on_cran()


test_that("crul POST requests works", {
  withr::local_options(warnPartialMatchDollar = FALSE)
  local_vcr_configure(dir = withr::local_tempdir())

  # body type: named list
  out <- use_cassette("crul_post_named_list", {
    x <- crul::HttpClient$new(hb("/post"))$post(body = list(foo = "bar"))
  })
  expect_false(out$is_empty())
  expect_s3_class(x, "HttpResponse")
  expect_equal(x$status_code, 200)
  str <- yaml::yaml.load_file(out$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$form, list(foo = "bar"))

  # body type: character
  out2 <- use_cassette("crul_post_string", {
    z <- crul::HttpClient$new(hb("/post"))$post(body = "some string")
  })
  expect_false(out2$is_empty())
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out2$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: raw
  out3 <- use_cassette("crul_post_raw", {
    z <- crul::HttpClient$new(hb("/post"))$post(body = charToRaw("some string"))
  })
  expect_false(out3$is_empty())
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out3$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  # FIXME: the body should be found in data slot, fix in crul
  expect_named(strj$form, "some string")

  # body type: upload_file
  ## upload_file in a list
  ff <- withr::local_tempfile(fileext = ".txt")
  cat("hello world\n", file = ff)
  out4 <- use_cassette("crul_post_upload_file", {
    b <- crul::HttpClient$new(hb_remote("/post"))$post(
      body = list(y = crul::upload(ff))
    )
  })
  expect_false(out4$is_empty())
  expect_s3_class(b, "HttpResponse")
  expect_equal(b$status_code, 200)
  str <- yaml::yaml.load_file(out4$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_match(strj$files$y, "hello world") # files not empty
  expect_false(nzchar(strj$data)) # data empty

  ## upload_file not in a list
  out6 <- use_cassette("crul_post_upload_file_no_list", {
    d <- crul::HttpClient$new(hb("/post"))$post(
      body = crul::upload(system.file("CITATION"))
    )
  })
  expect_false(out6$is_empty())
  expect_s3_class(d, "HttpResponse")
  expect_equal(d$status_code, 200)
  str <- yaml::yaml.load_file(out6$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(length(strj$files), 0) # files empty
  expect_match(strj$data, "bibentry\\(") # data not empty

  # body type: NULL
  out5 <- use_cassette("crul_post_null", {
    m <- crul::HttpClient$new(hb_remote("/post"))$post(body = NULL)
  })
  expect_false(out5$is_empty())
  expect_s3_class(z, "HttpResponse")
  expect_equal(z$status_code, 200)
  str <- yaml::yaml.load_file(out5$file())$http_interactions
  strj <- jsonlite::fromJSON(str[[1]]$response$body$string)
  expect_equal(strj$data, "")
  expect_equal(strj$headers$`Content-Length`, "0")
})
