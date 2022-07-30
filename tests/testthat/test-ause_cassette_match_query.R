
url <- "https://httpbin.org/get?update=2022-01-01T00:00:00&p2=ok"
req_crul <- function(url) {
    tmp <- crul::HttpClient$new(url)$get()
}
req_httr <- function(url) {
    tmp <- httr::GET(url)
}

test_that('request matching is not sensitive to escaping special characters', {
  skip_on_cran()
  skip_on_ci()
  # run twice the request with curl (curl does not escape)
  aa <- vcr::use_cassette('get_crul_match', {
    req_crul(url)
  }, match_requests_on = c("method", "uri", "query"))
  res <- req_crul(url)
  expect_true(res$status_code == 200)
  # run twice the request with httr (httr does escape on parameters)
  bb <- vcr::use_cassette('get_httr_match', {
    req_httr(url)
  }, match_requests_on = c("method", "uri", "query"))
  res <- req_httr(url)
  expect_true(res$status_code == 200)
})