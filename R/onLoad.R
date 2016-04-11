vcr_c <- NULL
.onLoad <- function(libname, pkgname){
  vcr_c <- VCRConfig$new()
  VCRHooks <- Hooks$new()
}
