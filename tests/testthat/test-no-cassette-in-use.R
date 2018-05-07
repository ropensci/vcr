context("no_cassette_in_use")

# dir <- tempdir()
# invisible(vcr_configure(dir = dir))
#
# test_that("no cassette in use behaves as expected", {
#   crul::mock()
#   x <- crul::HttpClient$new(url = "https://httpbin.org")
#
#   # when no cassette in use, we get expected vcr error
#   expect_error(
#     x$get("get"),
#     "There is currently no cassette in use"
#   )
# })
#
# # cleanup
# unlink(file.path(vcr_configuration()$dir, "turtle.yml"))
#
# # reset configuration
# vcr_configure_reset()
