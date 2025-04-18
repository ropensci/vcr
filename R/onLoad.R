vcr_c <- NULL # nocov start
VCRHooks <- NULL
the <- NULL

initialize_ivars <- function() {
  request_ignorer <<- RequestIgnorer$new()
}

.onLoad <- function(libname, pkgname) {
  # initialize vcr config object
  vcr_c <<- VCRConfig$new()
  # initialize hooks
  VCRHooks <<- Hooks$new()
  # initialize bucket of cassettes in session
  the <<- new.env(parent = emptyenv())
  the$cassettes <- list()
  the$light_switch <- lightswitch_init()
  the$last_error <- list()
  # lots of things
  initialize_ivars()
} # nocov end
