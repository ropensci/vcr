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
#' @param uri_parser the uri parser, default: \code{\link[httr]{parse_url}}
#' @param preserve_exact_body_bytes (logical) preserve exact body bytes for
#' @param preserve_exact_body_bytes_for (logical) preserve exact body bytes
#' @examples
#' vcr_configure(
#'  dir = "fixtures/vcr_cassettes",
#'  record = "once"
#' )
#' vcr_configuration()
vcr_configure <- function(
  dir = cassette_path(),
  record = "once",
  match_requests_on = "deafault_matchers",
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml",
  persist_with = "FileSystem",
  ignore_hosts = NULL,
  ignore_localhost = FALSE,
  ignore_request = NULL,
  uri_parser = "httr::parse_url",
  preserve_exact_body_bytes = FALSE,
  preserve_exact_body_bytes_for = FALSE) {

  Sys.setenv(vcr_config.dir = dir)
  Sys.setenv(vcr_config.record = record)
  Sys.setenv(vcr_config.uri_parser = uri_parser)
  Sys.setenv(vcr_config.preserve_exact_body_bytes = preserve_exact_body_bytes)
  Sys.setenv(vcr_config.preserve_exact_body_bytes_for = preserve_exact_body_bytes_for)
}

#' @export
#' @rdname vcr_configure
vcr_configuration <- function() {
  d <- Sys.getenv("vcr_config.dir", "")
  rec <- Sys.getenv("vcr_config.record", "")
  up <- Sys.getenv("vcr_config.uri_parser", "")
  pebb <- as.logical(Sys.getenv("vcr_config.preserve_exact_body_bytes", ""))
  pebbfor <- as.logical(Sys.getenv("vcr_config.preserve_exact_body_bytes_for", ""))
  structure(list(dir = d, record = rec, uri_parser = up,
                 preserve_exact_body_bytes = pebb,
                 preserve_exact_body_bytes_for = pebbfor), class = "vcr_config")
}

#' @export
print.vcr_config <- function(x) {
  cat("<vcr configuration>", sep = "\n")
  cat(paste0("  Cassette Dir: ", x$dir), sep = "\n")
  cat(paste0("  Record: ", x$record), sep = "\n")
  cat(paste0("  URI Parser: ", x$uri_parser))
}

# default_cassette_options <- list(
#   record = "once",
#   match_requests_on = "RequestMatcherRegistry::DEFAULT_MATCHERS",
#   allow_unused_http_interactions = TRUE,
#   serialize_with = "yaml",
#   persist_with = "file_system"
# )
