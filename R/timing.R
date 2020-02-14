#' timing helper
#' @noRd
#' @examples \dontrun{
#' # 300 ms = 0.3 s
#' vcr_configure(timing = list(fixed = 3000))
#' vcr_c$timing
#' system.time(vcr_timing())
#' }
vcr_timing <- function() {
  if (is.null(vcr_c$timing) || length(vcr_c$timing) == 0) return(NULL)
  if (is.list(vcr_c$timing)) {
    if (
      inherits(vcr_c$timing$fixed, c("numeric", "integer")) &&
      vcr_c$timing$fixed > 0
    ) {
      Sys.sleep(vcr_c$timing$fixed / 1000)
    }
  }
}
