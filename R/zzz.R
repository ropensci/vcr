pluck <- function(x, name, type) {
  if (missing(type)) {
    lapply(x, "[[", name)
  } else {
    vapply(x, "[[", name, FUN.VALUE = type)
  }
}

last <- function(x) x[length(x)][[1]]

#' Turns VCR off for the duration of a block.
#'
#' @export
#' @param options List of options
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#'  inserted while VCR is turned off. If \code{TRUE} is passed, the cassette insertion
#'  will be ignored; otherwise an error will be raised.
#' @seealso turn_off, turn_on, turned_on
turned_off <- function(options = list(), ignore_cassettes = FALSE){
  # turn_off!(options)
  # begin
  # yield
  # ensure
  # turn_on!
  #   end
}

turned_on <- function() TRUE

#' Turns VCR off, so that it no longer handles every HTTP request.
#'
#' @export
#' @param options List of options
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#'  inserted while VCR is turned off. If \code{TRUE} is passed, the cassette insertion
#'  will be ignored; otherwise an error will be raised.
turn_off <- function(cassette = NULL, options = list(), ignore_cassettes = FALSE){
  if (current_cassette) {
    stop("A VCR cassette is currently in use (#{VCR.current_cassette.name}).
         You must eject it before you can turn VCR off.", call. = FALSE)
  }
  if (any(invalid_options(options))) stop("You passed some invalid options", call. = FALSE)
  turn_off_()
}

turn_off_ <- function() message("VCR turned off; see ?turn_on to turn VCR back on")

errmssg <- "use_cassette requires a block.\nIf you cannot wrap your code in a block, use\ninsert_cassette / eject_cassette instead."
