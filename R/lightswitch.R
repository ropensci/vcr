#' Turn vcr on and off, check on/off status, and turn off for a given http call
#'
#' @export
#' @name lightswitch
#' @param ... Any block of code to run, presumably an http request
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#' inserted while vcr is turned off. If `TRUE` is passed, the cassette
#' insertion will be ignored; otherwise an error will be raised.
#' @details
#'
#' - `turned_off()` - Turns vcr off for the duration of a block.
#' - `turn_off()` - Turns vcr off, so that it no longer handles every
#'  HTTP request
#' - `turn_on()` - turns vcr on
#' - `turned_on()` - Asks if vcr is turned on, gives a boolean
#'
#' @examples \dontrun{
#' vcr_configure(dir = tempdir())
#'
#' turn_on()
#' turned_on()
#' turn_off()
#'
#' # turn off for duration of a block
#' library(crul)
#' turned_off({
#'  res <- HttpClient$new(url = "https://eu.httpbin.org/get")$get()
#' })
#' res
#'
#' # turn completely off
#' turn_off()
#' library(webmockr)
#' crul::mock()
#' # HttpClient$new(url = "https://eu.httpbin.org/get")$get(verbose = TRUE)
#' turn_on()
#' }
turned_off <- function(..., ignore_cassettes = FALSE) {
  turn_off(ignore_cassettes = ignore_cassettes)
  on.exit(turn_on())
  off_block(...)
}

off_block <- function(...) {
  tmp <- lazyeval::lazy_dots(...)
  xxx <- lazyeval::lazy_eval(tmp)
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
turn_off <- function(ignore_cassettes = FALSE) {
  cassette <- tryCatch(current_cassette(), error = function(e) e)
  if (!inherits(cassette, "error")) {
    if (length(cassette) != 0) {
      stop(
        sprintf(
          "A vcr cassette is currently in use: %s.\n  You must eject it before you can turn vcr off",
          cassette$name), call. = FALSE)
    }
  }
  vcr_c$ignore_cassettes <- ignore_cassettes
  message("vcr turned off; see ?turn_on to turn vcr back on")
  light_switch$turned_off <- TRUE
}
