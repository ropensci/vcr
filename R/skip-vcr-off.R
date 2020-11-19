#' Skip tests if vcr is off
#'
#' Custom testthat skipper to skip tests if vcr is turned off via the
#' environment variable `VCR_TURN_OFF`.
#'
#' @details This might be useful if your test will fail with real requests:
#' when the cassette was e.g. edited (a real request produced a 200 status code
#' but you made it a 502 status code for testing the behavior of your code
#' when the API errors)
#' or if the tests are very specific (e.g. testing a date was correctly parsed,
#' but making a real request would produce a different date).
#'
#' @return Nothing, skip test.
#' @export
#'
#' @seealso [turn_off()]
skip_if_vcr_off <- function() {

  if (!requireNamespace("testthat", quietly = TRUE)) {
    stop(
      paste0(
        "This function is meant to be used within testthat tests.",
        "Please install testthat."
      )
      )
  }

  if (
    nzchar(Sys.getenv("VCR_TURN_OFF")) &&
    as.logical(Sys.getenv("VCR_TURN_OFF"))
    ) {
    testthat::skip("Not run when vcr is off")
  }
}
