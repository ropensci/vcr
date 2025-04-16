#' Locate file in tests directory
#'
#' This function, similar to `testthat::test_path()`, is designed to work both
#' interactively and during tests, locating files in the `tests/` directory.
#'
#' @note `vcr_test_path()` assumes you are using testthat for your unit tests.
#'
#' @param ...	Character vectors giving path component. each character string
#' gets added on to the path, e.g., `vcr_test_path("a", "b")` becomes
#' `tests/a/b` relative to the root of the package.
#'
#' @return A character vector giving the path
#' @export
#' @examples
#' if (interactive()) {
#' vcr_test_path("fixtures")
#' }
vcr_test_path <- function(...) {
  if (missing(...)) stop("Please provide a directory name.")
  if (any(!nzchar(...))) stop("Please use non empty path elements.")

  # dirname () moves up one level from testthat dir
  root <- dirname(rprojroot::find_testthat_root_file())
  path <- file.path(root, ...)
  if (!dir.exists(path)) {
    message("could not find ", path, "; creating it")
    dir_create(path)
  }
  path
}
