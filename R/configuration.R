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
#' @param ignore_request List of requests to ignore. NOT USED RIGHT NOW, sorry
#' @param uri_parser the uri parser, default: [crul::url_parse()]
#' @param preserve_exact_body_bytes (logical) preserve exact body bytes for
#' @param turned_off (logical) VCR is turned on by default. Default:
#' `FALSE`
#' @param re_record_interval (numeric) When given, the cassette will be
#' re-recorded at the given interval, in seconds.
#' @param clean_outdated_http_interactions (logical) Should outdated interactions
#' be recorded back to file. Default: `FALSE`
#' @param allow_http_connections_when_no_cassette (logical) Determines how vcr
#' treats HTTP requests that are made when no vcr cassette is in use. When
#' `TRUE`, requests made when there is no vcr cassette in use will be allowed.
#' When `FALSE` (default), an [UnhandledHTTPRequestError] error will be raised
#' for any HTTP request made when there is no cassette in use
#' @param cassettes (list) don't use
#' @param linked_context (logical) linked context
#' @param log (logical) should we log important vcr things? Default: `FALSE`
#' @param log_opts (list) Additional logging options. Options include:
#'
#' - file: one of a file path to log to or "console"
#' - log_prefix: default: "Cassette". We insert the cassette name after
#' that prefix, then the rest of the message
#' - more to come
#'
#' @param filter_sensitive_data (list) named list of values to replace. format
#' is: `list(thing_to_replace_it_with = thing_to_replace)`. We replace all
#' instances of `thing_to_replace` with `thing_to_replace_it_with`. Before
#' recording (writing to a cassette) we do the replacement and then when
#' reading from the cassette we do the reverse replacement to get back
#' to the real data. Before record replacement happens in internal
#' function `write_interactions()`, while before playback replacement
#' happens in internal function `YAML$deserialize_path()`
#' 
#' @param write_disk_path (character) path to write files to 
#' for any requests that write responses to disk. by default this parameter
#' is `NULL`. For testing a package, you'll probably want this path to 
#' be in your `tests/` directory, perhaps next to your cassettes
#' directory, e.g., where your cassettes are in `tests/fixtures`, your
#' files from requests that write to disk are in `tests/files`
#'
#' @examples
#' vcr_configure(dir = tempdir())
#' vcr_configure(dir = tempdir(), record = "all")
#' vcr_configuration()
#' vcr_config_defaults()
#' vcr_configure(tempdir(), ignore_hosts = "google.com")
#' vcr_configure(tempdir(), ignore_localhost = TRUE)
#'
#' # logging
#' vcr_configure(tempdir(), log = TRUE, 
#'   log_opts = list(file = file.path(tempdir(), "vcr.log")))
#' vcr_configure(tempdir(), log = TRUE, log_opts = list(file = "console"))
#' vcr_configure(tempdir(), log = TRUE,
#'  log_opts = list(
#'    file = file.path(tempdir(), "vcr.log"), 
#'    log_prefix = "foobar"
#' ))
#' vcr_configure(tempdir(), log = FALSE)
#'
#' # filter sensitive data
#' vcr_configure(tempdir(), 
#'   filter_sensitive_data = list(foo = "<bar>")
#' )
#' vcr_configure(tempdir(), 
#'   filter_sensitive_data = list(foo = "<bar>", hello = "<world>")
#' )
vcr_configure <- function(
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
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  allow_http_connections_when_no_cassette = FALSE,
  cassettes = list(),
  linked_context = NULL,
  log = FALSE,
  log_opts = list(file = "vcr.log", log_prefix = "Cassette", date = TRUE),
  filter_sensitive_data = NULL,
  write_disk_path = NULL
  ) {

  record <- check_record_mode(record)
  match_requests_on <- check_request_matchers(match_requests_on)

  assert(log, "logical")
  assert(log_opts, "list")
  assert(filter_sensitive_data, "list")
  if (length(log_opts) > 0) {
    if ("file" %in% names(log_opts)) {
      assert(log_opts$file, "character")
      if (log) vcr_log_file(log_opts$file)
    }
    if ("log_prefix" %in% names(log_opts)) {
      assert(log_opts$log_prefix, "character")
    }
    if ("date" %in% names(log_opts)) {
      assert(log_opts$date, "logical")
    }
  }

  assert(ignore_hosts, "character")
  assert(ignore_localhost, "logical")
  if (!is.null(ignore_hosts) || ignore_localhost) {
    x <- RequestIgnorer$new()
    if (!is.null(ignore_hosts)) x$ignore_hosts(hosts = ignore_hosts)
    if (ignore_localhost) x$ignore_localhost()
  }

  # add missing log options
  log_opts <- merge_list(log_opts,
    list(file = "vcr.log", log_prefix = "Cassette", date = TRUE))

  calls <- as.list(environment(), all.names = TRUE)
  calls$x <- NULL
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
    re_record_interval = NULL,
    clean_outdated_http_interactions = NULL,
    allow_http_connections_when_no_cassette = NULL,
    cassettes = NULL,
    linked_context = NULL,
    log = NULL,
    log_opts = NULL,
    filter_sensitive_data = NULL,
    write_disk_path = NULL,

    print = function(...) {
      cat("<vcr configuration>", sep = "\n")
      cat(paste0("  Cassette Dir: ", self$dir), sep = "\n")
      cat(paste0("  Record: ", self$record), sep = "\n")
      cat(paste0("  URI Parser: ", self$uri_parser), sep = "\n")
      cat(paste0("  Match Requests on: ",
        pastec(self$match_requests_on)), sep = "\n")
      cat(paste0("  Preserve Bytes?: ",
        self$preserve_exact_body_bytes), sep = "\n")
      logloc <- if (self$log) sprintf(" (%s)", self$log_opts$file) else ""
      cat(paste0("  Logging?: ", self$log, logloc), sep = "\n")
      cat(paste0("  ignored hosts: ", pastec(self$ignore_hosts)), sep = "\n")
      cat(paste0("  ignore localhost?: ", self$ignore_localhost), sep = "\n")
      cat(paste0("  Write disk path: ", self$write_disk_path), sep = "\n")
      invisible(self)
    },

    defaults = function() {
      vcr_default_config_vars
    }
  )
)

pastec <- function(x) paste0(x, collapse = ", ")

vcr_default_config_vars <- list(
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
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL,
  allow_http_connections_when_no_cassette = FALSE,
  cassettes = list(),
  linked_context = NULL,
  log = FALSE,
  log_opts = list(file = "vcr.log", log_prefix = "Cassette"),
  filter_sensitive_data = NULL,
  write_disk_path = NULL
)
