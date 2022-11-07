test_that('issue 249 is correctly handled.', {
  vcr::use_cassette('get_401', {
    res <- httr::GET('https://httpbin.org/status/401')
  })
  expect_true(res$status_code == 401)
})
