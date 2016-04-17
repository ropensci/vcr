#' Configuration
#'
#' @export
#' @param dir Cassette directory
#' @param record (character) On of 'all', 'none', 'new_episodes', or 'once'.
#' See \link{recording}.
#' @param match_requests_on vector of matchers. Default: (\code{method}, \code{uri})
#' @param allow_unused_http_interactions (logical) Default: \code{TRUE}
#' @param serialize_with (character) Right now only "yaml"
#' @param persist_with (character) Right now only "FileSystem"
#' @param ignore_hosts Vector of hosts to ignore
#' @param ignore_localhost (logical) Default: \code{FALSE}
#' @param ignore_request Lists of requests to ignore
#' @param uri_parser the uri parser, default: \code{\link[httr]{parse_url}}
#' @param preserve_exact_body_bytes (logical) preserve exact body bytes for
#' @param preserve_exact_body_bytes_for (logical) preserve exact body bytes
#' @param turned_off (logical) VCR is turned on by default. Default: \code{FALSE}
#' @param ignore_cassettes (logical) Ignore cassettes. You can set this to \code{TRUE}
#' when you don't have a cassette in use but still want to make HTTP requests. Otherwise,
#' you can't make requests unless a cassette is in use. Default: \code{FALSE}
#' @param cassettes (list) don't use
#' @param linked_context (logical) linked context
#' @param vcr_logging (character) one of a file path to log to, "console", or "stdout"
#' @param ... (list) Additional options passed to \code{\link[loggr]{log_file}}
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
  dir = cassette_path(),
  record = "once",
  match_requests_on = c("method", "uri"),
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml",
  persist_with = "FileSystem",
  ignore_hosts = NULL,
  ignore_localhost = FALSE,
  ignore_request = NULL,
  uri_parser = "httr::parse_url",
  preserve_exact_body_bytes = FALSE,
  preserve_exact_body_bytes_for = FALSE,
  turned_off = FALSE,
  ignore_cassettes = FALSE,
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
    preserve_exact_body_bytes_for = NULL,
    turned_off = NULL,
    ignore_cassettes = NULL,
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
      invisible(self)
    },

    defaults = function() {
      vcr_default_config_vars
    }
  )
)

vcr_default_config_vars <- list(
  dir = cassette_path(),
  record = "once",
  match_requests_on = c("method", "uri"),
  allow_unused_http_interactions = TRUE,
  serialize_with = "yaml",
  persist_with = "FileSystem",
  ignore_hosts = NULL,
  ignore_localhost = FALSE,
  ignore_request = NULL,
  uri_parser = "httr::parse_url",
  preserve_exact_body_bytes = FALSE,
  preserve_exact_body_bytes_for = FALSE,
  turned_off = FALSE,
  ignore_cassettes = FALSE,
  cassettes = list(),
  linked_context = NULL,
  vcr_logging = "vcr.log",
  vcr_logging_opts = list()
)
