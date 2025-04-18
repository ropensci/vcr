vcr_c <- NULL # nocov start
VCRHooks <- NULL
request_matchers <- NULL
request_ignorer <- NULL
light_switch <- NULL
the <- NULL

initialize_ivars <- function() {
  light_switch <<- list2env(lightswitch_init(), parent = emptyenv())

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
  # lots of things
  initialize_ivars()
} # nocov end
