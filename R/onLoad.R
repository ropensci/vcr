vcr_c <- NULL # nocov start
request_matchers <- NULL
light_switch <- NULL
the <- NULL

initialize_ivars <- function() {
  light_switch <<- list2env(lightswitch_init(), parent = emptyenv())
  vcr_c$cassettes <<- list()
  vcr_c$linked_context <<- NULL
  request_matchers <<- RequestMatcherRegistry$new()
}

.onLoad <- function(libname, pkgname) {
  # initialize vcr config object
  vcr_c <<- VCRConfig$new()
  # initialize bucket of cassettes in session
  the <<- new.env(parent = emptyenv())
  the$cassettes <- list()
  the$request_ignorer <<- RequestIgnorer$new()
  # lots of things
  initialize_ivars()
} # nocov end
