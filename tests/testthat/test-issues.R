test_that('issue 249 is correctly handled.', {
  use_cassette('get_401', {
    res <- httr::GET(hb_remote('/status/401'))
  })
  expect_true(res$status_code == 401)
})
