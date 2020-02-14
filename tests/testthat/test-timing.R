test_that("vcr_timing() helper fxn", {
  skip_on_cran()
  
  # negative number ignored
  invisible(vcr_configure(timing = list(fixed = -5)))
  expect_equal(system.time(vcr_timing())[['elapsed']], 0)

  # zero/0
  invisible(vcr_configure(timing = list(fixed = 0)))
  expect_equal(system.time(vcr_timing())[['elapsed']], 0)

  # 1000 ms = 1 sec
  invisible(vcr_configure(timing = list(fixed = 1000)))
  expect_equal(round(system.time(vcr_timing())[['elapsed']], 1), 1)
})

test_that("timing configuration", {
  skip_on_cran()

  vcr_configure_reset()

  # default: fixed set to 0, relative NULL
  invisible(vcr_configure())
  expect_equal(vcr_c$timing$fixed, 0)
  expect_null(vcr_c$timing$relative)
  
  invisible(vcr_configure(timing = list(fixed = 3000)))
  expect_equal(vcr_c$timing$fixed, 3000)
  expect_null(vcr_c$timing$relative)
})

test_that("vcr_configuration timing fails well", {
  skip_on_cran()
  
  expect_error(vcr_configure(timing = list(fixed = "asdf")))
  expect_error(vcr_configure(timing = list(relative = 3)), "not working yet")
  # FIXME: add test later if we figure out the relative option
  ## we can't allow both to be set
  # expect_error(vcr_configure(timing = list(fixed = 30, relative = 3)))
})

test_that("timing works as expected", {
  skip_on_cran()

  vcr_configure_reset()

  library(crul)
  mydir <- file.path(tempdir(), "foofoobarbar")
  invisible(vcr_configure(dir = mydir, timing = list(fixed = 3000)))
  unlink(file.path(vcr_c$dir, "foofoobarbar.yml"))
  cli <- HttpClient$new(url = "https://httpbin.org")

  aa <- use_cassette(name = "foofoobarbar", {
    res <- cli$get("get", query = list(foo = "bar"))
  }, match_requests_on = c("method", "uri"))

  # with timing set
  expect_equal(round(system.time(
    use_cassette(name = "foofoobarbar", {
      res <- cli$get("get", query = list(foo = "bar"))
    }, match_requests_on = c("method", "uri"))
  )[['elapsed']], 0), 3)
  
  # without timing set
  invisible(vcr_configure(dir = mydir, timing = list(fixed = 0)))
  expect_lt(round(system.time(
    use_cassette(name = "foofoobarbar", {
      res <- cli$get("get", query = list(foo = "bar"))
    }, match_requests_on = c("method", "uri"))
  )[['elapsed']], 0), 3)
})
