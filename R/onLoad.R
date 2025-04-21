# nocov start
vcr_c <- NULL
the <- NULL

.onLoad <- function(libname, pkgname) {
  # initialize vcr config object
  vcr_c <<- VCRConfig$new()
  # initialize bucket of cassettes in session
  the <<- new.env(parent = emptyenv())
  the$cassettes <- list()
  the$light_switch <- lightswitch_init()
  the$last_error <- list()
} # nocov end
