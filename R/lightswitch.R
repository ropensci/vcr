#' Turn vcr on and off
#'
#' * `turn_on()` and `turn_off()` turn on and off for the whole session.
#' * `turned_off(code)` temporarily turns off while `code` is running.
#' * `turned_on()` reports on if vcr is turned on or not.
#' * `skip_if_vcr_off()` skips a test if vcr is turned off. This is
#'   occassionally useful if you're using a cassette to simulate a faked
#'   request, or if the real request would return differents values (e.g.
#'   you're testing date parsing and the request returns the current date).
#'
#' @export
#' @name lightswitch
#' @param code Any block of code to run, presumably an http request
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
#'  res <- HttpClient$new(url = "https://hb.opencpu.org/get")$get()
#' })
#' res
#'
#' # turn completely off
#' turn_off()
#' library(webmockr)
#' crul::mock()
#' # HttpClient$new(url = "https://hb.opencpu.org/get")$get(verbose = TRUE)
#' turn_on()
#' }
turn_on <- function() {
  the$light_switch$on <- TRUE
  invisible()
}

#' @export
#' @rdname lightswitch
turn_off <- function(ignore_cassettes = FALSE) {
  cassette <- current_cassette()
  if (!is.null(cassette)) {
    cli::cli_abort("You must eject all cassettes before you can turn vcr off.")
  }

  the$light_switch$ignore_cassettes <- ignore_cassettes
  the$light_switch$on <- FALSE
  message("vcr turned off; see ?turn_on to turn vcr back on")

  invisible()
}

#' @rdname lightswitch
#' @export
turned_off <- function(code, ignore_cassettes = FALSE) {
  suppressMessages(turn_off(ignore_cassettes = ignore_cassettes))
  on.exit(turn_on())

  code
}

#' @rdname lightswitch
#' @export
turned_on <- function() {
  the$light_switch$on
}

#' @rdname lightswitch
#' @export
skip_if_vcr_off <- function() {
  check_required("testthat")
  if (!turned_on()) {
    testthat::skip("vcr is turned off")
  }
  invisible()
}

# Initial values from env vars ------------------------------------------------

lightswitch_init <- function() {
  list(
    on = !(get_envvar_lgl("VCR_TURNED_OFF") %||%
      get_envvar_lgl("VCR_TURN_OFF") %||%
      FALSE),
    ignore_cassettes = get_envvar_lgl("VCR_IGNORE_CASSETTES") %||%
      get_envvar_lgl("VCR_TURN_OFF") %||%
      FALSE
  )
}

get_envvar_lgl <- function(var, default = NULL, error_call = caller_env()) {
  val <- Sys.getenv(var, "")
  if (val == "") {
    return(default)
  }

  lgl <- as.logical(val)
  if (identical(lgl, NA)) {
    cli::cli_abort(
      "env var {.var {var}} must be TRUE or FALSE, not {.str {val}}.",
      call = error_call
    )
  }

  lgl
}
