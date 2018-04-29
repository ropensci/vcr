context("write_interactions")

# body <- readRDS(file = "~/d1xmlbody.rds")
# body <- substring(body, 1, 3000)
# nchar(body)
request <- Request$new("POST", uri = 'https://eu.httpbin.org/post?a=5',
  body = "", headers = list(foo = "bar"))
status <- list(status_code = "200", message = "OK",
               explanation = "Request fulfilled, document follows")
# resp_body <- body
resp_body <- "asdf jasljf asdfjlsaf"
response <- VcrResponse$new(status, list(this_is_my = "response_header"),
                            resp_body, "HTTP/1.1 200 OK")
x <- HTTPInteraction$new(request = request, response = response)
x <- x$to_hash()

tfile <- tempfile(fileext = ".yml")

test_that("write_interactions fails well", {
  # missing
  expect_error(write_interactions(), "argument \"x\" is missing")
  expect_error(write_interactions(bytes = TRUE), "argument \"x\" is missing")

  # types
  expect_error(write_interactions(5, bytes = TRUE),
               "x must be of class list")
  expect_error(write_interactions(x, 4, bytes = TRUE),
               "file must be of class character")

})

test_that("write_interactions works as expected", {
  write_interactions(x, tfile, FALSE)
  txt <- readLines(tfile)
  # sum(nchar(txt))
  expect_is(txt, "character")
  expect_gt(length(txt), 10)
  expect_true(any(grepl("- request", txt)))
  expect_true(any(grepl("method: post", txt)))
  expect_true(any(grepl("this_is_my: response_header", txt)))

  # yaml reading works
  yml <- yaml::yaml.load_file(tfile)
  expect_is(yml, "list")
  expect_equal(length(yml), 1)
  expect_is(yml[[1]], "list")
  expect_named(yml[[1]], c('request', 'response',
                           'recorded_at', 'recorded_with'))
})

# cleanup
unlink(tfile)
