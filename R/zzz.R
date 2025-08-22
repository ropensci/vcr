# nocov start
the <- NULL

.onLoad <- function(libname, pkgname) {
  the <<- new.env(parent = emptyenv())
  the$config <- vcr_config_defaults()
  the$cassettes <- list()
  the$light_switch <- lightswitch_init()
  the$last_request <- NULL
  the$last_response <- NULL
  s3_register("roxygen2::roxy_tag_parse", "roxy_tag_examplesVCR")
  s3_register("roxygen2::roxy_tag_rd", "roxy_tag_examplesVCR")
  s3_register("roxygen2::roclet_process", "roclet_examplesVCR")
  s3_register("roxygen2::roclet_output", "roclet_examplesVCR")
} # nocov end
