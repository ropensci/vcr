#' Global Configuration Options
#'
#' Configurable options that define vcr's default behavior.
#'
#' @param ... configuration settings used to override defaults. See below for a
#'   complete list of valid arguments.
#'
#' @section Configurable settings:
#'
#' ## vcr options
#'
#' ### File locations
#'
#' - `dir` Cassette directory
#' - `write_disk_path` (character) path to write files to
#' for any requests that write responses to disk. by default this parameter
#' is `NULL`. For testing a package, you'll probably want this path to
#' be in your `tests/` directory, perhaps next to your cassettes
#' directory, e.g., where your cassettes are in `tests/fixtures`, your
#' files from requests that write to disk are in `tests/files`.
#' If you want to ignore these files in your installed package,
#' add them to `.Rinstignore`. If you want these files ignored on build
#' then add them to `.Rbuildignore` (though if you do, tests that depend
#' on these files probably will not work because they won't be found; so
#' you'll likely have to skip the associated tests as well).
#'
#' ### Contexts
#'
#' - `turned_off` (logical) VCR is turned on by default. Default:
#' `FALSE`
#' - `allow_unused_http_interactions` (logical) Default: `TRUE`
#' - `allow_http_connections_when_no_cassette` (logical) Determines how vcr
#' treats HTTP requests that are made when no vcr cassette is in use. When
#' `TRUE`, requests made when there is no vcr cassette in use will be allowed.
#' When `FALSE` (default), an [UnhandledHTTPRequestError] error will be raised
#' for any HTTP request made when there is no cassette in use
#'
#' ### Filtering
#'
#' - `ignore_hosts` (character) Vector of hosts to ignore. e.g., localhost, or
#' google.com. These hosts are ignored and real HTTP requests allowed to go
#' through
#' - `ignore_localhost` (logical) Default: `FALSE`
#' - `ignore_request` List of requests to ignore. NOT USED RIGHT NOW, sorry
#' - `filter_sensitive_data` named list of values to replace. Format is:
#'   ```
#'   list(thing_to_replace_it_with = thing_to_replace)
#'   ```
#'   We replace all instances of `thing_to_replace` with
#' `thing_to_replace_it_with`. Uses [gsub()] internally, with `fixed=TRUE`;
#' so does exact matches. Before recording (writing to a cassette) we do
#' the replacement and then when reading from the cassette we do the reverse
#' replacement to get back to the real data. Before record replacement happens
#' in internal function `write_interactions()`, while before playback
#' replacement happens in internal function `YAML$deserialize()`
#'
#' - `filter_sensitive_data_regex` named list of values to replace. Follows
#' `filter_sensitive_data` format, except uses `fixed=FALSE` in the [gsub()]
#' function call; this means that the value in `thing_to_replace` is a regex
#' pattern.
#'
#' - `filter_request_headers` (character/list) **request** headers to filter.
#' A character vector of request headers to remove - the headers will not be
#' recorded to disk. Alternatively, a named list similar to
#' `filter_sensitive_data` instructing vcr with what value to replace the
#' real value of the request header. Note that for the `httr2` package only
#' we redact request headers automatically that are marked (via attributes)
#' as redacted.
#' - `filter_response_headers` (named list) **response** headers to filter.
#' A character vector of response headers to remove - the headers will not be
#' recorded to disk. Alternatively, a named list similar to
#' `filter_sensitive_data` instructing vcr with what value to replace the
#' real value of the response header.
#' - `filter_query_parameters` (named list) query parameters to filter.
#' A character vector of query parameters to remove - the query parameters
#' will not be recorded to disk. Alternatively, a named list similar to
#' `filter_sensitive_data` instructing vcr with what value to replace the
#' real value of the query parameter.
#'
#' ## Errors
#'
#' - `verbose_errors` Do you want more verbose errors or less verbose
#' errors when cassette recording/usage fails? Default is `FALSE`, that is,
#' less verbose errors. If `TRUE`, error messages will include more details
#' about what went wrong and suggest possible solutions. For testing
#' in an interactive R session, if `verbose_errors=FALSE`, you can run
#' `vcr_last_error()` to get the full error. If in non-interactive mode,
#' which most users will be in when running the entire test suite for a
#' package, you can set an environment variable (`VCR_VERBOSE_ERRORS`)
#' to toggle this setting (e.g.,
#' `Sys.setenv(VCR_VERBOSE_ERRORS=TRUE); devtools::test()`)
#'
#' ### Internals
#'
#' - `cassettes` (list) don't use
#' - `linked_context` (logical) linked context
#' - `uri_parser` the uri parser, default: `crul::url_parse()`
#'
#' ### Logging
#'
#' - `log` (logical) should we log important vcr things? Default: `FALSE`
#' - `log_opts` (list) Additional logging options:
#'   - 'file' either `"console"` or a file path to log to
#'   - 'log_prefix' default: "Cassette". We insert the cassette name after
#'     that prefix, then the rest of the message.
#'   - More to come...
#'
#' ## Cassette Options
#'
#' These settings can be configured globally, using `vcr_configure()`, or
#' locally, using either `use_cassette()` or `insert_cassette()`. Global
#' settings are applied to *all* cassettes but are overridden by settings
#' defined locally for individual cassettes.
#'
#' - `record` (character) One of 'all', 'none', 'new_episodes', or 'once'.
#' See [recording]
#' - `match_requests_on` vector of matchers. Default: (`method`, `uri`)
#' See [request-matching] for details.
#' - `serialize_with`: (character) "yaml" or "json". Note that you can have
#' multiple cassettes with the same name as long as they use different
#' serializers; so if you only want one cassette for a given cassette name,
#' make sure to not switch serializers, or clean up files you no longer need.
#' - `json_pretty`: (logical) want JSON to be newline separated to be easier
#' to read? Or remove newlines to save disk space? default: FALSE
#' - `preserve_exact_body_bytes` (logical) preserve exact body bytes for
#' - `re_record_interval` (numeric) When given, the cassette will be
#' re-recorded at the given interval, in seconds.
#' - `clean_outdated_http_interactions` (logical) Should outdated interactions
#' be recorded back to file. Default: `FALSE`
#' - `quiet` (logical) Suppress any messages from both vcr and webmockr.
#' Default: `TRUE`
#' - `warn_on_empty_cassette` (logical) Should a warning be thrown when an
#' empty cassette is detected? Empty cassettes are cleaned up (deleted) either
#' way. This option only determines whether a warning is thrown or not.
#' Default: `FALSE`
#'
#' @examples
#' vcr_configure(dir = tempdir())
#' vcr_configure(dir = tempdir(), record = "all")
#' vcr_configuration()
#' vcr_config_defaults()
#' vcr_configure(dir = tempdir(), ignore_hosts = "google.com")
#' vcr_configure(dir = tempdir(), ignore_localhost = TRUE)
#'
#'
#' # logging
#' vcr_configure(dir = tempdir(), log = TRUE,
#'   log_opts = list(file = file.path(tempdir(), "vcr.log")))
#' vcr_configure(dir = tempdir(), log = TRUE, log_opts = list(file = "console"))
#' vcr_configure(dir = tempdir(), log = TRUE,
#'  log_opts = list(
#'    file = file.path(tempdir(), "vcr.log"),
#'    log_prefix = "foobar"
#' ))
#' vcr_configure(dir = tempdir(), log = FALSE)
#'
#' # filter sensitive data
#' vcr_configure(dir = tempdir(),
#'   filter_sensitive_data = list(foo = "<bar>")
#' )
#' vcr_configure(dir = tempdir(),
#'   filter_sensitive_data = list(foo = "<bar>", hello = "<world>")
#' )
#' @export

vcr_configure <- function(...) {
  params <- list2(...)

  invalid <- !names(params) %in% vcr_c$fields()
  if (any(invalid)) {
    warning(
      "The following configuration parameters are not valid:",
      sprintf("\n  * %s", params[invalid]),
      call. = FALSE
    )
    params <- params[!invalid]
  }

  if (length(params) == 0) return(invisible(vcr_c$as_list()))

  # TODO: Is this still the right place to change these settings?
  ignore_hosts <- params$ignore_hosts
  ignore_localhost <- params$ignore_localhost %||% FALSE
  if (!is.null(ignore_hosts) || ignore_localhost) {
    x <- RequestIgnorer$new()
    if (!is.null(ignore_hosts)) x$ignore_hosts(hosts = ignore_hosts)
    if (ignore_localhost) x$ignore_localhost()
  }

  old <- vcr_c$as_list()[names(params)]

  for (i in seq_along(params)) {
    vcr_c[[names(params)[i]]] <- params[[i]]
  }

  invisible(old)
}

#' @export
#' @rdname vcr_configure
#' @inheritParams local_cassette
local_vcr_configure <- function(..., .frame = parent.frame()) {
  old <- vcr_configure(...)
  withr::defer(vcr_configure(!!!old), envir = .frame)
  invisible()
}


#' @export
#' @rdname vcr_configure
vcr_configure_reset <- function() vcr_c$reset()

#' @export
#' @rdname vcr_configure
vcr_configuration <- function() vcr_c

#' @export
#' @rdname vcr_configure
vcr_config_defaults <- function() VCRConfig$new()$as_list()

VCRConfig <- R6::R6Class(
  "VCRConfig",

  private = list(
    .dir = NULL,
    .record = NULL,
    .match_requests_on = NULL,
    .allow_unused_http_interactions = NULL,
    .serialize_with = NULL,
    .json_pretty = NULL,
    .ignore_hosts = NULL,
    .ignore_localhost = NULL,
    .ignore_request = NULL,
    .uri_parser = NULL,
    .preserve_exact_body_bytes = NULL,
    .turned_off = NULL,
    .re_record_interval = NULL,
    .clean_outdated_http_interactions = NULL,
    .allow_http_connections_when_no_cassette = NULL,
    .cassettes = NULL,
    .linked_context = NULL,
    .log = NULL,
    .log_opts = NULL,
    .filter_sensitive_data = NULL,
    .filter_sensitive_data_regex = NULL,
    .filter_request_headers = NULL,
    .filter_response_headers = NULL,
    .filter_query_parameters = NULL,
    .write_disk_path = NULL,
    .verbose_errors = NULL,
    .quiet = NULL,
    .warn_on_empty_cassette = NULL
  ),

  active = list(
    dir = function(value) {
      if (missing(value)) return(private$.dir)
      private$.dir <- value
    },
    record = function(value) {
      if (missing(value)) return(private$.record)
      private$.record <- check_record_mode(value)
    },
    match_requests_on = function(value) {
      if (missing(value)) return(private$.match_requests_on)
      private$.match_requests_on <- check_request_matchers(value)
    },
    allow_unused_http_interactions = function(value) {
      if (missing(value)) return(private$.allow_unused_http_interactions)
      private$.allow_unused_http_interactions <- value
    },
    serialize_with = function(value) {
      if (missing(value)) return(private$.serialize_with)
      private$.serialize_with <- value
    },
    json_pretty = function(value) {
      if (missing(value)) return(private$.json_pretty)
      private$.json_pretty <- value
    },
    ignore_hosts = function(value) {
      if (missing(value)) return(private$.ignore_hosts)
      private$.ignore_hosts <- assert(value, "character")
    },
    ignore_localhost = function(value) {
      if (missing(value)) return(private$.ignore_localhost)
      private$.ignore_localhost <- assert(value, "logical")
    },
    ignore_request = function(value) {
      if (missing(value)) return(private$.ignore_request)
      private$.ignore_request <- value
    },
    uri_parser = function(value) {
      if (missing(value)) return(private$.uri_parser)
      private$.uri_parser <- value
    },
    preserve_exact_body_bytes = function(value) {
      if (missing(value)) return(private$.preserve_exact_body_bytes)
      private$.preserve_exact_body_bytes <- value
    },
    turned_off = function(value) {
      if (missing(value)) return(private$.turned_off)
      private$.turned_off <- value
    },
    re_record_interval = function(value) {
      if (missing(value)) return(private$.re_record_interval)
      private$.re_record_interval <- value
    },
    clean_outdated_http_interactions = function(value) {
      if (missing(value)) return(private$.clean_outdated_http_interactions)
      private$.clean_outdated_http_interactions <- value
    },
    allow_http_connections_when_no_cassette = function(value) {
      if (missing(value))
        return(private$.allow_http_connections_when_no_cassette)
      private$.allow_http_connections_when_no_cassette <- value
    },
    cassettes = function(value) {
      if (missing(value)) return(private$.cassettes)
      private$.cassettes <- value
    },
    linked_context = function(value) {
      if (missing(value)) return(private$.linked_context)
      private$.linked_context <- value
    },
    log = function(value) {
      if (missing(value)) return(private$.log)
      private$.log <- assert(value, "logical")
    },
    log_opts = function(value) {
      if (missing(value)) return(private$.log_opts)
      log_opts <- assert(value, "list")
      if (length(log_opts) > 0) {
        if ("file" %in% names(log_opts)) {
          assert(log_opts$file, "character")
          if (private$.log) vcr_log_file(log_opts$file)
        }
        if ("log_prefix" %in% names(log_opts)) {
          assert(log_opts$log_prefix, "character")
        }
        if ("date" %in% names(log_opts)) {
          assert(log_opts$date, "logical")
        }
      }
      # add missing log options
      log_opts <- merge_list(
        log_opts,
        list(file = "vcr.log", log_prefix = "Cassette", date = TRUE)
      )
      private$.log_opts <- log_opts
    },
    filter_sensitive_data = function(value) {
      if (missing(value)) return(private$.filter_sensitive_data)
      private$.filter_sensitive_data <- assert(value, "list")
    },
    filter_sensitive_data_regex = function(value) {
      if (missing(value)) return(private$.filter_sensitive_data_regex)
      private$.filter_sensitive_data_regex <- assert(value, "list")
    },
    filter_request_headers = function(value) {
      if (missing(value)) return(private$.filter_request_headers)
      if (is.character(value)) value <- as.list(value)
      private$.filter_request_headers <- assert(value, "list")
    },
    filter_response_headers = function(value) {
      if (missing(value)) return(private$.filter_response_headers)
      if (is.character(value)) value <- as.list(value)
      private$.filter_response_headers <- assert(value, "list")
    },
    filter_query_parameters = function(value) {
      if (missing(value)) return(private$.filter_query_parameters)
      if (is.character(value)) value <- as.list(value)
      lapply(value, function(w) {
        if (!length(w) %in% 0:2)
          stop("filter query values must be of length 1 or 2", call. = FALSE)
      })
      private$.filter_query_parameters <- assert(value, "list")
    },
    write_disk_path = function(value) {
      if (missing(value)) return(private$.write_disk_path)
      private$.write_disk_path <- value
    },
    verbose_errors = function(value) {
      if (missing(value)) return(private$.verbose_errors)
      private$.verbose_errors <- value
    },
    quiet = function(value) {
      if (missing(value)) return(private$.quiet)
      private$.quiet <- assert(value, "logical")
    },
    warn_on_empty_cassette = function(value) {
      if (missing(value)) return(private$.warn_on_empty_cassette)
      private$.warn_on_empty_cassette <- assert(value, "logical")
    }
  ),

  public = list(
    initialize = function(
      dir = ".",
      record = "once",
      match_requests_on = c("method", "uri"),
      allow_unused_http_interactions = TRUE,
      serialize_with = "yaml",
      json_pretty = FALSE,
      ignore_hosts = NULL,
      ignore_localhost = FALSE,
      ignore_request = NULL,
      uri_parser = "crul::url_parse",
      preserve_exact_body_bytes = FALSE,
      turned_off = FALSE,
      re_record_interval = NULL,
      clean_outdated_http_interactions = FALSE,
      allow_http_connections_when_no_cassette = FALSE,
      cassettes = list(),
      linked_context = NULL,
      log = FALSE,
      log_opts = list(file = "vcr.log", log_prefix = "Cassette", date = TRUE),
      filter_sensitive_data = NULL,
      filter_sensitive_data_regex = NULL,
      filter_request_headers = NULL,
      filter_response_headers = NULL,
      filter_query_parameters = NULL,
      write_disk_path = NULL,
      verbose_errors = get_envvar_lgl("VCR_VERBOSE_ERRORS", FALSE),
      quiet = TRUE,
      warn_on_empty_cassette = TRUE
    ) {
      self$dir <- dir
      self$record <- record
      self$match_requests_on <- match_requests_on
      self$allow_unused_http_interactions <- allow_unused_http_interactions
      self$serialize_with <- serialize_with
      self$json_pretty <- json_pretty
      self$ignore_hosts <- ignore_hosts
      self$ignore_localhost <- ignore_localhost
      self$ignore_request <- ignore_request
      self$uri_parser <- uri_parser
      self$preserve_exact_body_bytes <- preserve_exact_body_bytes
      self$turned_off <- turned_off
      self$re_record_interval <- re_record_interval
      self$clean_outdated_http_interactions <- clean_outdated_http_interactions
      self$allow_http_connections_when_no_cassette <- allow_http_connections_when_no_cassette
      self$cassettes <- cassettes
      self$linked_context <- linked_context
      self$log <- log
      self$log_opts <- log_opts
      self$filter_sensitive_data <- filter_sensitive_data
      self$filter_sensitive_data_regex <- filter_sensitive_data_regex
      self$filter_request_headers = filter_request_headers
      self$filter_response_headers = filter_response_headers
      self$filter_query_parameters = filter_query_parameters
      self$write_disk_path <- write_disk_path
      self$verbose_errors <- verbose_errors
      self$quiet <- quiet
      self$warn_on_empty_cassette <- warn_on_empty_cassette
    },

    # reset all settings to defaults
    reset = function() self$initialize(),

    # print out names of configurable settings
    fields = function() sub("^\\.", "", names(private)),

    # return current configuration as a list
    as_list = function() {
      stats::setNames(mget(names(private), private), self$fields())
    },

    print = function(...) {
      cat("<vcr configuration>", sep = "\n")
      cat(paste0("  Cassette Dir: ", private$.dir), sep = "\n")
      cat(paste0("  Record: ", private$.record), sep = "\n")
      cat(paste0("  Serialize with: ", private$.serialize_with), sep = "\n")
      cat(paste0("  URI Parser: ", private$.uri_parser), sep = "\n")
      cat(
        paste0("  Match Requests on: ", pastec(private$.match_requests_on)),
        sep = "\n"
      )
      cat(
        paste0("  Preserve Bytes?: ", private$.preserve_exact_body_bytes),
        sep = "\n"
      )
      logloc <- if (private$.log) sprintf(" (%s)", private$.log_opts$file) else
        ""
      cat(paste0("  Logging?: ", private$.log, logloc), sep = "\n")
      cat(
        paste0("  ignored hosts: ", pastec(private$.ignore_hosts)),
        sep = "\n"
      )
      cat(
        paste0("  ignore localhost?: ", private$.ignore_localhost),
        sep = "\n"
      )
      cat(paste0("  Write disk path: ", private$.write_disk_path), sep = "\n")
      invisible(self)
    }
  )
)

pastec <- function(x) paste0(x, collapse = ", ")
