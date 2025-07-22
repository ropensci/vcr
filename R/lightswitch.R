#' Turn vcr on and off
#'
#' @description
#'
#' * `turn_on()` and `turn_off()` turn on and off for the whole session.
#' * `turned_off(code)` temporarily turns off while `code` is running,
#'   guaranteeing that you make a real HTTP request.
#' * `turned_on()` reports on if vcr is turned on or not.
#' * `skip_if_vcr_off()` skips a test if vcr is turned off. This is
#'   occasionally useful if you're using a cassette to simulate a faked
#'   request, or if the real request would return different values (e.g.
#'   you're testing date parsing and the request returns the current date).
#'
#' You can also control the default behaviour in a new session by setting the
#' following environment variables before R starts:
#'
#' * Use `VCR_TURN_OFF=true` to suppress all vcr usage, ignoring all
#'   cassettes. This is useful for CI/CD workflows where you want to ensure
#'   the test suite is run against the live API.
#' * Set `VCR_TURNED_OFF=true` to turn off vcr, but still use cassettes.
#'
#' @export
#' @name lightswitch
#' @param code Any block of code to run, presumably an HTTP request.
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#' inserted while vcr is turned off. If `TRUE` is passed, the cassette
#' insertion will be ignored; otherwise an error will be raised.
#' Default: `FALSE`
#' @examples
#' # By default, vcr is turned on
#' turned_on()
#'
#' # you can turn off for the rest of the session
#' turn_off()
#' turned_on()
#' # turn on again
#' turn_on()
#'
#' # or just turn it on turn off temporarily
#' turned_off({
#'   # some HTTP requests here
#'   turned_on()
#' })
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
  defer(turn_on())

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

vcr_turned_off <- function(error_call = caller_env()) {
  if (the$light_switch$on) {
    return(FALSE)
  }

  if (!the$light_switch$ignore_cassettes) {
    cli::cli_abort(
      c(
        "vcr is turned off.",
        i = "Use {.fun turn_on} to turn it back on.",
        i = "Or use {.code turn_off(ignore_cassettes = TRUE)} to ignore cassettes completely."
      ),
      call = error_call
    )
  } else {
    TRUE
  }
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
