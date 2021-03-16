#' Locate file in tests directory
#'
#' This function, similar to `testthat::test_path()`, is designed to work both
#' interactively and during tests, locating files in the `tests/` directory.
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
  root <- rprojroot::is_r_package
  path <- root$find_file("tests", ...)
  if (!dir.exists(path)){
    message("could not find ", path, "; creating it")
    dir.create(path)
  }
  path
}
