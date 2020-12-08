#' Turn vcr on and off, check on/off status, and turn off for a given http call
#'
#' @export
#' @name lightswitch
#' @param ... Any block of code to run, presumably an http request
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#' inserted while vcr is turned off. If `TRUE` is passed, the cassette
#' insertion will be ignored; otherwise an error will be raised.
#' Default: `FALSE`
#' @includeRmd man/rmdhunks/lightswitch.Rmd
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
  force(...)
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
