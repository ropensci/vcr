# nocov start
the <- NULL

.onLoad <- function(libname, pkgname) {
  the <<- new.env(parent = emptyenv())
  the$config <- vcr_config_defaults()
  the$cassettes <- list()
  the$light_switch <- lightswitch_init()
  the$last_request <- NULL
  the$last_response <- NULL
} # nocov end
