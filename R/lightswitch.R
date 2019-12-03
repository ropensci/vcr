#' Turn vcr on and off, check on/off status, and turn off for a given http call
#'
#' @export
#' @name lightswitch
#' @param ... Any block of code to run, presumably an http request
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#' inserted while vcr is turned off. If `TRUE` is passed, the cassette
#' insertion will be ignored; otherwise an error will be raised.
#' Default: `FALSE`
#' @details
#'
#' - `turned_off()` - Turns vcr off for the duration of a block.
#' - `turn_off()` - Turns vcr off, so that it no longer handles every
#'  HTTP request
#' - `turn_on()` - turns vcr on
#' - `turned_on()` - Asks if vcr is turned on, gives a boolean
#'
#' To turn vcr off completely, for example, if you are using vcr in your
#' package, but you want to run real HTTP requests in your tests, there are
#' a few options:
#'
#' - Run `turn_off(ignore_cassettes = TRUE)` before running tests. You can
#' do this on the command line e.g.,
#' `Rscript -e 'vcr::turn_off(TRUE); devtools::test()'`, or within a running
#' R session the same way.
#' - Set an environment variable `VCR_TURN_OFF=TRUE`.
#' You can do this on the command line by setting the env var at the beginning
#' of the line like: `VCR_TURN_OFF=TRUE Rscript -e 'devtools::test()'`. Same
#' can be done within an interactive R session. You can also use this approach
#' to turn on or off vcr in CI builds like on Travis or Appveyor by setting
#' this env var in your Travis/Appveyor configuration file or in the settings
#' windows in the respective web apps
#' 
#' The full set of environment variables `vcr` uses, all of which accept only
#' `TRUE` or `FALSE`:
#' 
#' - `VCR_TURN_OFF`: turn off vcr altogether; set to `TRUE` to skip any vcr
#' usage; default: `FALSE`
#' - `VCR_TURNED_OFF`: set the `turned_off` internal package setting; this
#' does not turn off vcr completely as does `VCR_TURN_OFF` does, but rather
#' is looked at together with `VCR_IGNORE_CASSETTES`
#' - `VCR_IGNORE_CASSETTES`: set the `ignore_cassettes` internal package
#' setting; this is looked at together with `VCR_TURNED_OFF`
#' 
#' See `?Startup` if you're not sure how to set environment variables
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
  light_switch$ignore_cassettes <- ignore_cassettes
  message("vcr turned off; see ?turn_on to turn vcr back on")
  light_switch$turned_off <- TRUE
}

# environment variable handlers
vcr_env_mssg <- "invalid option for env var: '%s'; see ?vcr::lightswitch"
vcr_env_handle <- function() {
  vcr_env_turn_off()
  vcr_env_turned_off()
  vcr_env_ignore_cassettes()
}
catch_error <- function(x) tryCatch(x, error = function(e) e)
vcr_env_var_check <- function(x, var) {
  if (!inherits(x, "logical") || length(x) != 1 || all(is.na(x)))
    stop(sprintf(vcr_env_mssg, var), call. = FALSE)
}
vcr_env_turn_off <- function() {
  var <- "VCR_TURN_OFF"
  x <- Sys.getenv(var, "")
  if (x != "") {
    x <- as.logical(x)
    vcr_env_var_check(x, var)
    light_switch$ignore_cassettes <- x
    light_switch$turned_off <- x
  }
}
vcr_env_turned_off <- function() {
  var <- "VCR_TURNED_OFF"
  x <- Sys.getenv(var, "")
  if (x != "") {
    x <- as.logical(x)
    vcr_env_var_check(x, var)
    light_switch$turned_off <- x
  }
}
vcr_env_ignore_cassettes <- function() {
  var <- "VCR_IGNORE_CASSETTES"
  x <- Sys.getenv(var, "")
  if (x != "") {
    x <- as.logical(x)
    vcr_env_var_check(x, var)
    light_switch$ignore_cassettes <- x
  }
}
