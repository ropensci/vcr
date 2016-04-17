#' Record a call to a block of code
#'
#' @noRd
#' @template common
#' @keywords internal
#' @examples \dontrun{
#' cassette <- cassettes()[[1]]
#' call_block(cassette, {
#'   GET("http://google.com")
#' })
#' }

call_block <- function(cassette, ...) {
  call_block_(cassette, ...)
}

call_block_ <- function(cassette, ...){
  tmp <- lazyeval::lazy_dots(...)
  out <- eval(tmp[[1]]$expr)
  cassette <- as.cassette(cassette)
  ## FIXME - presumably, need to remove this write here
  ##   and just record somewhere in the R session
  write_cassette(cassette, out)
  return( out )
}
