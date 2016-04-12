vcr_c <- NULL
VCRHooks <- NULL
request_matchers <- NULL
request_ignorer <- NULL
cassette_serializers <- NULL
cassette_persisters <- NULL
light_switch <- NULL

initialize_ivars <- function() {
  light_switch <<- new.env()
  light_switch$turned_off <<- FALSE
  vcr_c$ignore_cassettes <<- FALSE
  vcr_c$cassettes <<- list()
  vcr_c$linked_context <<- NULL

  request_matchers <<- RequestMatcherRegistry$new()
  request_ignorer <<- RequestIgnorer$new()
  cassette_serializers <<- Serializers$new()
  cassette_persisters <<- Persisters$new()
}

.onLoad <- function(libname, pkgname){
  vcr_c <<- VCRConfig$new()
  vcr_configure()
  VCRHooks <<- Hooks$new()
  initialize_ivars()
}
