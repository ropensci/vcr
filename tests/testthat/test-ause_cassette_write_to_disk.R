vcr_configure_reset()
tmpdir_wdp <- file.path(tempdir(), "write_disk_path")
vcr_configure(dir = tmpdir_wdp)

test_that("fails well if write_disk_path not set", {
  skip_on_cran()

  expect_null(vcr_c$write_disk_path)

  f <- tempfile(fileext = ".json")
  expect_error(
    sw(use_cassette(
      "write_disk_path_not_set_crul",
      out <- crul::HttpClient$new(hb("/get"))$get(disk = f)
    )),
    "write_disk_path must be given"
  )

  g <- tempfile(fileext = ".json")
  expect_error(
    sw(use_cassette(
      "write_disk_path_not_set_httr",
      out <- httr::GET(hb("/get"), httr::write_disk(g, TRUE))
    )),
    "write_disk_path must be given"
  )
})

# cleanup
files <- c(
  "write_disk_path_not_set_crul.yml",
  "write_disk_path_not_set_httr.yml"
)
unlink(file.path(vcr_configuration()$dir, files))

tmpdir <- tempdir()
vcr_configure(dir = tmpdir, write_disk_path = file.path(tmpdir, "files"))

test_that("use_cassette w/ request that writes to disk: crul", {
  skip_on_cran()

  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make a request
  use_cassette(
    "test_write_to_disk",
    out <- crul::HttpClient$new(hb("/get"))$get(disk = f)
  )

  expect_s3_class(out, "HttpResponse")
  expect_type(out$content, "character")
  expect_match(out$content, "\\.json")
  expect_type(out$parse(), "character")

  # works on 2nd request
  use_cassette(
    "test_write_to_disk",
    out2 <- crul::HttpClient$new(hb("/get"))$get(disk = f)
  )
  expect_s3_class(out2, "HttpResponse")
  expect_type(out2$content, "character")
  expect_match(out2$content, "\\.json")
  expect_type(out2$parse(), "character")

  expect_equal(out$parse(), out2$parse())
})

test_that("use_cassette w/ request that writes to disk: httr", {
  skip_on_cran()

  ## make a temp file
  f <- tempfile(fileext = ".json")
  ## make a request
  use_cassette(
    "test_write_to_disk_httr",
    out <- httr::GET(hb("/get"), httr::write_disk(f, TRUE))
  )

  expect_s3_class(out, "response")
  expect_s3_class(out$content, "path")
  expect_match(out$content, "\\.json")

  # works on 2nd request
  use_cassette(
    "test_write_to_disk_httr",
    out2 <- httr::GET(hb("/get"), httr::write_disk(f, TRUE))
  )
  expect_s3_class(out2, "response")
  expect_s3_class(out2$content, "path")
  expect_match(out2$content, "\\.json")

  expect_equal(httr::content(out), httr::content(out2))
})

# cleanup
files <- c("test_write_to_disk.yml", "test_write_to_disk_httr.yml")
unlink(file.path(vcr_configuration()$dir, files))

# reset configuration
vcr_configure_reset()
