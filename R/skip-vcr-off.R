#' Skip tests if vcr is off
#'
#' Custom testthat skippers to skip tests if vcr is turned off via the
#' environment variable `VCR_TURN_OFF`.
#'
#' @return Nothing, skip test.
#' @export
#'
#' @seealso lightswitch
skip_if_vcr_off <- function() {

  if (is(try(find.package("testthat"), silent = TRUE), "try-error")) {
    stop("This function is meant to be use within testthat tests. Install testthat.")
  }

  if (nzchar(Sys.getenv("VCR_TURN_OFF")) && as.logical(Sys.getenv("VCR_TURN_OFF"))) {
    testthat::skip("Not run when vcr is off")
  }
}
