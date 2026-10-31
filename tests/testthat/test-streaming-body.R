test_that("StreamingBodyReplay returns recorded chunks one at a time", {
  body <- StreamingBodyReplay$new(list(as.raw(1:3), as.raw(4:5)))
  expect_s3_class(body, "StreamingBody")
  expect_true(body$is_open())
  expect_false(body$is_complete())

  expect_equal(body$read(100), as.raw(1:3))
  expect_false(body$is_complete())
  expect_equal(body$read(100), as.raw(4:5))
  expect_true(body$is_complete())
  expect_equal(body$read(100), raw())
})

test_that("StreamingBodyReplay splits chunks that are larger than `n`", {
  body <- StreamingBodyReplay$new(list(as.raw(1:5)))
  expect_equal(body$read(2), as.raw(1:2))
  expect_equal(body$read(2), as.raw(3:4))
  expect_false(body$is_complete())
  expect_equal(body$read(2), as.raw(5))
  expect_true(body$is_complete())
})

test_that("StreamingBodyReplay read_all() reads everything and closes", {
  body <- StreamingBodyReplay$new(list(as.raw(1:3), as.raw(4:5)))
  expect_equal(body$read_all(), as.raw(1:5))
  expect_false(body$is_open())

  empty <- StreamingBodyReplay$new(list())
  expect_true(empty$is_complete())
  expect_null(empty$read_all())
})

test_that("StreamingBodyReplay returns nothing once closed", {
  body <- StreamingBodyReplay$new(list(as.raw(1:3)))
  body$close()
  expect_false(body$is_open())
  expect_equal(body$read(10), raw())
})

test_that("split_body_chunks() splits by recorded sizes", {
  body <- charToRaw("abcdefghij")
  expect_equal(
    split_body_chunks(body, c(3, 5, 2)),
    list(charToRaw("abc"), charToRaw("defgh"), charToRaw("ij"))
  )
  expect_equal(split_body_chunks(raw()), list())
})

test_that("split_body_chunks() falls back to one chunk when sizes don't fit", {
  body <- charToRaw("abcdefghij")
  expect_equal(split_body_chunks(body, NULL), list(body))
  expect_equal(split_body_chunks(body, c(3, 5)), list(body))
  expect_equal(split_body_chunks(body, c(3, 9)), list(body))
})

test_that("read_stream_chunks() collects chunks and closes the stream", {
  body <- StreamingBodyReplay$new(list(as.raw(1:3), as.raw(4:5)))
  expect_equal(read_stream_chunks(body), list(as.raw(1:3), as.raw(4:5)))
  expect_false(body$is_open())
})
