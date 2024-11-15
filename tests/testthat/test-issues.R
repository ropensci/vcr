test_that('issue 249 is correctly handled.', {
  vcr::use_cassette('get_401', {
    res <- httr::GET(hb('/status/401'))
  })
  expect_true(res$status_code == 401)
})
