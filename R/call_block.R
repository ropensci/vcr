#' Record a call to a block of code
#'
#' @noRd
#' @template common
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
  # out
  # out <- eval(block)
  cassette <- as.cassette(cassette)
  write_cassette(cassette, out)
  return( out )
}
