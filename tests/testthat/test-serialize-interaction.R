test_that("header/body only included if needed", {
  interaction <- vcr_interaction(
    vcr_request(
      method = "GET",
      uri = "http://example.com",
      body = "body",
      headers = list(x = 1, y = 2)
    ),
    vcr_response(status = 200L, list(), NULL)
  )

  encoded <- encode_interaction(interaction, matchers = c("method", "uri"))
  expect_named(encoded$request, c("method", "uri"))
})
