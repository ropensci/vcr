#' Turn vcr on and off, check on/off status, and turn off for a given http call
#'
#' @export
#' @name lightswitch
#' @param ... Any block of code to run, presumably an http request
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#'  inserted while vcr is turned off. If \code{TRUE} is passed, the cassette insertion
#'  will be ignored; otherwise an error will be raised.
#' @details
#' \itemize{
#'  \item turned_off - Turns vcr off for the duration of a block.
#'  \item turn_off - Turns vcr off, so that it no longer handles every HTTP request.
#'  \item turn_on - turns vcr on
#'  \item turned_on - Asks if vcr is turned on, gives a boolean
#' }
#' @examples \dontrun{
#' turn_on()
#' turned_on()
#' turn_off()
#' turned_off(
#'  httr::GET("http://httpbin.org/get")
#' )
#' }
turned_off <- function(..., ignore_cassettes = FALSE){
  turn_off(ignore_cassettes = ignore_cassettes)
  on.exit(turn_on())
  off_block(...)
}

off_block <- function(...) {
  tmp <- lazyeval::lazy_dots(...)
  out <- eval(tmp[[1]]$expr)
  return( out )
}

#' @rdname lightswitch
#' @export
turn_on <- function() {
  light_switch$turned_off <- FALSE
}

#' @rdname lightswitch
#' @export
turned_on <- function() {
  !light_switch$turned_off
}

#' @export
#' @rdname lightswitch
turn_off <- function(ignore_cassettes = FALSE){
  cassette <- cassette_current()
  if (length(cassette) != 0) {
    stop(
      sprintf(
        "A vcr cassette is currently in use: %s.\n  You must eject it before you can turn vcr off",
        cassette$name), call. = FALSE)
  }
  message("vcr turned off; see ?turn_on to turn vcr back on")
  light_switch$turned_off <- TRUE
}
