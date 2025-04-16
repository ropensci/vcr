vcr_c <- NULL # nocov start
VCRHooks <- NULL
request_matchers <- NULL
request_ignorer <- NULL
cassette_serializers <- NULL
light_switch <- NULL
vcr_cassettes <- NULL

initialize_ivars <- function() {
  light_switch <<- list2env(lightswitch_init(), parent = emptyenv())
  vcr_c$cassettes <<- list()
  vcr_c$linked_context <<- NULL

  request_matchers <<- RequestMatcherRegistry$new()
  request_ignorer <<- RequestIgnorer$new()
  cassette_serializers <<- Serializers$new()
}

.onLoad <- function(libname, pkgname) {
  # initialize vcr config object
  vcr_c <<- VCRConfig$new()
  # initialize hooks
  VCRHooks <<- Hooks$new()
  # initialize bucket of cassettes in session
  vcr_cassettes <<- new.env()
  # lots of things
  initialize_ivars()
} # nocov end
