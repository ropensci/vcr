vcr_c <- NULL # nocov start
VCRHooks <- NULL
request_matchers <- NULL
request_ignorer <- NULL
the <- NULL

initialize_ivars <- function() {
  vcr_c$cassettes <<- list()
  vcr_c$linked_context <<- NULL

  request_matchers <<- RequestMatcherRegistry$new()
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
  # lots of things
  initialize_ivars()
} # nocov end
