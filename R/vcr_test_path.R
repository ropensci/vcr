#' Locate file in tests directory
#'
#' This function, similar to `testthat::test_path()`, is designed to work both
#' interactively and during tests, locating files in the `tests/` directory.
#'
#' @param ...	Character vectors giving path component. each character string
#' gets added on to the path, e.g., `vcr_test_path("a", "b")` becomes
#' `tests/a/b` when run from the root of the package.
#'
#' @return A character vector giving the path
#' @export
#' @examples
#' if (interactive()) {
#' vcr_test_path("fixtures")
#' }
# Adapted from https://github.com/r-lib/testthat/blob/45a9c705402bd51af29b9d999e587ba789f6203f/R/test-path.R#L1
vcr_test_path <- function(...) {

  if (any(!nzchar(...))) {
    stop("Please use non empty path elements.")
  }

  if (identical(Sys.getenv("TESTTHAT"), "true") &&
      !isTRUE(getOption("testthat_interactive"))) {
    if (missing(...)) {
      "../."
    }
    else {
      file.path("..", ...)
    }
  }
  else {
    base <- "tests"
    if (!dir.exists(base)) {
      stop("Can't find `tests` in current directory.")
    }
    file.path(base, ...)
  }
}
