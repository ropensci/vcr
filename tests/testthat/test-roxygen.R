test_that("@examplesVCR", {
  local_mocked_bindings(
    current_package = function(...) "my.package"
  )

  out <- roxygen2::roc_proc_text(
    roxygen2::rd_roclet(),
    "
    #' @name a
    #' @title a
    #' @examplesVCR bla
    #' httr2::request('http://r-project.org')
    NULL"
  )[[1]]

  expect_snapshot(out$get_section("examples"))
})
