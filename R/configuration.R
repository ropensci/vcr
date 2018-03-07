#' Configuration
#'
#' @export
#' @param dir Cassette directory
#' @param record (character) One of 'all', 'none', 'new_episodes', or 'once'.
#' See [recording]
#' @param match_requests_on vector of matchers. Default: (`method`, `uri`)
#' See [request-matching] for details.
#' @param allow_unused_http_interactions (logical) Default: `TRUE`
#' @param serialize_with (character) only option is "yaml"
#' @param persist_with (character) only option is "FileSystem"
#' @param ignore_hosts (character) Vector of hosts to ignore. e.g., localhost, or
#' google.com. These hosts are ignored and real HTTP requests allowed to go
#' through
#' @param ignore_localhost (logical) Default: `FALSE`
#' @param ignore_request List of requests to ignore
#' @param uri_parser the uri parser, default: [crul::url_parse()]
#' @param preserve_exact_body_bytes (logical) preserve exact body bytes for
#' @param turned_off (logical) VCR is turned on by default. Default:
#' `FALSE`
#' @param ignore_cassettes (logical) Ignore cassettes. You can set this to
#' `TRUE` when you don't have a cassette in use but still want to make
#' HTTP requests. Otherwise, you can't make requests unless a cassette is in
#' use. Default: `FALSE`
#' @param re_record_interval (numeric) When given, the cassette will be
#' re-recorded at the given interval, in seconds.
#' @param clean_outdated_http_interactions (logical) Should outdated interactions
#' be recorded back to file. Default: `FALSE`
#' @param cassettes (list) don't use
#' @param linked_context (logical) linked context
#' @param vcr_logging (character) one of a file path to log to, "console",
#' or "stdout"
#' @param vcr_logging_opts (list) Additional options passed to
#' `loggr::log_file()` (ignored for now)
#'
#' @examples \dontrun{
#' vcr_configure()
#' vcr_configure(
#'  dir = "fixtures/vcr_cassettes",
#'  record = "all"
#' )
#' vcr_configuration()
#' vcr_config_defaults()
#'
#' vcr_configure(
#'  ignore_localhost = TRUE
#' )
#' }
vcr_configure <- function(
  #dir = cassette_path(),
  dir = ".",
  record = "once",
  match_requests_on = c("method", "uri"),
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml",
  persist_with = "FileSystem",
  ignore_hosts = NULL,
  ignore_localhost = FALSE,
  ignore_request = NULL,
  uri_parser = "crul::url_parse",
  preserve_exact_body_bytes = FALSE,
  turned_off = FALSE,
  ignore_cassettes = FALSE,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  cassettes = list(),
  linked_context = NULL,
  vcr_logging = "vcr.log",
  vcr_logging_opts = list()) {

  calls <- as.list(environment(), all = TRUE)
  for (i in seq_along(calls)) {
    vcr_c[[names(calls)[i]]] <- calls[[i]]
  }
  return(vcr_c)
}

#' @export
#' @rdname vcr_configure
vcr_configure_reset <- function() vcr_configure()

#' @export
#' @rdname vcr_configure
vcr_configuration <- function() vcr_c

#' @export
#' @rdname vcr_configure
vcr_config_defaults <- function() vcr_c$defaults()

VCRConfig <- R6::R6Class(
  'VCRConfig',
  public = list(
    dir = NULL,
    record = NULL,
    match_requests_on = NULL,
    allow_unused_http_interactions = NULL,
    serialize_with = NULL,
    persist_with = NULL,
    ignore_hosts = NULL,
    ignore_localhost = NULL,
    ignore_request = NULL,
    uri_parser = NULL,
    preserve_exact_body_bytes = NULL,
    turned_off = NULL,
    ignore_cassettes = NULL,
    re_record_interval = NULL,
    clean_outdated_http_interactions = NULL,
    cassettes = NULL,
    linked_context = NULL,
    vcr_logging = NULL,
    vcr_logging_opts = NULL,

    print = function(...) {
      cat("<vcr configuration>", sep = "\n")
      cat(paste0("  Cassette Dir: ", self$dir), sep = "\n")
      cat(paste0("  Record: ", self$record), sep = "\n")
      cat(paste0("  URI Parser: ", self$uri_parser), sep = "\n")
      cat(paste0("  Match Requests on: ",
                 paste0(self$match_requests_on, collapse = ", ")), sep = "\n")
      cat(paste0("  Preserve Bytes?: ", self$preserve_exact_body_bytes), sep = "\n")
      invisible(self)
    },

    defaults = function() {
      vcr_default_config_vars
    }
  )
)

vcr_default_config_vars <- list(
  #dir = cassette_path(),
  dir = ".",
  record = "once",
  match_requests_on = c("method", "uri"),
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml",
  persist_with = "FileSystem",
  ignore_hosts = NULL,
  ignore_localhost = FALSE,
  ignore_request = NULL,
  uri_parser = "crul::url_parse",
  preserve_exact_body_bytes = FALSE,
  turned_off = FALSE,
  ignore_cassettes = FALSE,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  cassettes = list(),
  linked_context = NULL,
  vcr_logging = "vcr.log",
  vcr_logging_opts = list()
)
