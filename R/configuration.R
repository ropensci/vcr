#' Configuration
#'
#' @export
#' @param dir Cassette directory
#' @param record (character) On of once, ...
#' @param match_requests_on xx
#' @param allow_unused_http_interactions (logical) Default: \code{TRUE}
#' @param serialize_with (character) Right now only "yaml"
#' @param persist_with (character) Right now only "FileSystem"
#' @param ignore_hosts List of hosts to ignore
#' @param ignore_localhost (logical) Default: \code{FALSE}
#' @param ignore_request Lists of requests to ignore
#' @examples
#' vcr_configure(
#'  dir = "fixtures/vcr_cassettes",
#'  record = "once"
#' )
#' vcr_configuration()
vcr_configure <- function(dir = cassette_path(), record = "once",
  match_requests_on = "deafault_matchers",
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml", persist_with = "FileSystem",
  ignore_hosts = NULL, ignore_localhost = FALSE, ignore_request = NULL) {

  Sys.setenv(vcr_config.dir = dir)
  Sys.setenv(vcr_config.record = record)
}

#' @export
#' @rdname vcr_configure
vcr_configuration <- function() {
  d <- Sys.getenv("vcr_config.dir", "")
  rec <- Sys.getenv("vcr_config.record", "")
  structure(list(dir = d, record = rec), class = "vcr_config")
}

#' @export
print.vcr_config <- function(x) {
  cat("<vcr configuration>", sep = "\n")
  cat(paste0("  Cassette Dir: ", x$dir), sep = "\n")
  cat(paste0("  Record: ", x$record))
}

# default_cassette_options <- list(
#   record = "once",
#   match_requests_on = "RequestMatcherRegistry::DEFAULT_MATCHERS",
#   allow_unused_http_interactions = TRUE,
#   serialize_with = "yaml",
#   persist_with = "file_system"
# )
