#' Global Configuration Options
#'
#' Configurable options that define vcr's default behavior.
#'
#' @param ... Configuration settings used to override defaults.
#' @param dir Directory where cassettes are stored.
#' @param log,log_opts See [vcr_configure_log()].
#' @param write_disk_path (character) path to write files to
#'   for any requests that write responses to disk.  By default this will be
#'   `{cassette-name}-files/` inside the cassette directory.
#' @param turned_off (logical) VCR is turned on by default. Default:
#'   `FALSE`.
#' @param ignore_hosts (character) Vector of hosts to ignore. e.g.,
#'   `"localhost"`, or `"google.com"`. These hosts are ignored and real HTTP
#'   requests are allowed to go through.
#' @param ignore_localhost (logical) Default: `FALSE`
#' @param ignore_request List of requests to ignore. NOT USED RIGHT NOW, sorry
#' @param filter_sensitive_data named list of values to replace. Format is:
#'     ```
#'     list(thing_to_replace_it_with = thing_to_replace)
#'     ```
#'   We replace all instances of `thing_to_replace` with
#'   `thing_to_replace_it_with`. Uses [gsub()] internally, with `fixed=TRUE`;
#'   so does exact matches. Before recording (writing to a cassette) we do
#'   the replacement and then when reading from the cassette we do the reverse
#'   replacement to get back to the real data. Before record replacement happens
#'   in internal function `write_interactions()`, while before playback
#'   replacement happens in internal function `YAML$deserialize()`
#' @param filter_sensitive_data_regex named list of values to replace. Follows
#'   `filter_sensitive_data` format, except uses `fixed=FALSE` in the [gsub()]
#'   function call; this means that the value in `thing_to_replace` is a regex
#'   pattern.
#' @param filter_request_headers (character/list) **request** headers to filter.
#'   A character vector of request headers to remove - the headers will not be
#'   recorded to disk. Alternatively, a named list similar to
#' @param filter_sensitive_data instructing vcr with what value to replace the
#'   real value of the request header. Note that for the `httr2` package only
#'   we redact request headers automatically that are marked (via attributes)
#'   as redacted.
#' @param filter_response_headers (character/list) **response** headers to filter.
#'   A character vector of response headers to remove - the headers will not be
#'   recorded to disk. Alternatively, a named list similar to
#'   `filter_sensitive_data` instructing vcr with what value to replace the
#'   real value of the response header.
#' @param filter_query_parameters (named list) query parameters to filter.
#'   A character vector of query parameters to remove - the query parameters
#'   will not be recorded to disk. Alternatively, a named list similar to
#' @param filter_sensitive_data instructing vcr with what value to replace the
#'   real value of the query parameter.
#' @param verbose_errors Do you want more verbose errors or less verbose
#'  errors when cassette recording/usage fails? Default is `FALSE`, that is,
#'  less verbose errors. If `TRUE`, error messages will include more details
#'  about what went wrong and suggest possible solutions. For testing
#'  in an interactive R session, if `verbose_errors=FALSE`, you can run
#'  `vcr_last_error()` to get the full error. If in non-interactive mode,
#'  which most users will be in when running the entire test suite for a
#'  package, you can set an environment variable (`VCR_VERBOSE_ERRORS`)
#'  to toggle this setting (e.g.,
#'  `Sys.setenv(VCR_VERBOSE_ERRORS=TRUE); devtools::test()`)
#' @inheritParams use_cassette
#' @param json_pretty (logical) want JSON to be newline separated to be easier
#'   to read? Or remove newlines to save disk space? default: `FALSE`.`
#' @param warn_on_empty_cassette (logical) Should a warning be thrown when an
#'   empty cassette is detected? Empty cassettes are cleaned up (deleted) either
#'   way. This option only determines whether a warning is thrown or not.
#'   Default: `FALSE`
#' @examples
#' vcr_configure(dir = tempdir())
#' vcr_configure(dir = tempdir(), record = "all")
#' vcr_configuration()
#' vcr_config_defaults()
#' vcr_configure(dir = tempdir(), ignore_hosts = "google.com")
#' vcr_configure(dir = tempdir(), ignore_localhost = TRUE)
#'
#' # filter sensitive data
#' vcr_configure(dir = tempdir(),
#'   filter_sensitive_data = list(foo = "<bar>")
#' )
#' vcr_configure(dir = tempdir(),
#'   filter_sensitive_data = list(foo = "<bar>", hello = "<world>")
#' )
#' @export
vcr_configure <- function(
  dir,
  record,
  match_requests_on,
  serialize_with,
  json_pretty,
  ignore_hosts,
  ignore_localhost,
  ignore_request,
  preserve_exact_body_bytes,
  turned_off,
  re_record_interval,
  log,
  log_opts,
  filter_sensitive_data,
  filter_sensitive_data_regex,
  filter_request_headers,
  filter_response_headers,
  filter_query_parameters,
  write_disk_path,
  verbose_errors,
  warn_on_empty_cassette
) {
  # Get non-missing arguments
  new_params <- list()

  if (!missing(dir)) {
    check_string(dir, allow_null = TRUE)
    new_params["dir"] <- list(dir)
  }
  if (!missing(record)) {
    check_record_mode(record)
    new_params["record"] <- list(record)
  }
  if (!missing(match_requests_on)) {
    check_request_matchers(match_requests_on)
    new_params["match_requests_on"] <- list(match_requests_on)
  }
  if (!missing(serialize_with)) {
    check_string(serialize_with, allow_null = TRUE)
    new_params["serialize_with"] <- list(serialize_with)
  }
  if (!missing(json_pretty)) {
    check_bool(json_pretty, allow_null = TRUE)
    new_params["json_pretty"] <- list(json_pretty)
  }
  if (!missing(ignore_hosts)) {
    check_character(ignore_hosts, allow_null = TRUE)
    new_params["ignore_hosts"] <- list(ignore_hosts)
  }
  if (!missing(ignore_localhost)) {
    check_bool(ignore_localhost, allow_null = TRUE)
    new_params["ignore_localhost"] <- list(ignore_localhost)
  }
  if (!missing(ignore_request)) {
    new_params["ignore_request"] <- list(ignore_request)
  }
  if (!missing(preserve_exact_body_bytes)) {
    check_bool(preserve_exact_body_bytes, allow_null = TRUE)
    new_params["preserve_exact_body_bytes"] <- list(preserve_exact_body_bytes)
  }
  if (!missing(turned_off)) {
    check_bool(turned_off, allow_null = TRUE)
    new_params["turned_off"] <- list(turned_off)
  }
  if (!missing(re_record_interval)) {
    check_number_decimal(re_record_interval, allow_null = TRUE)
    new_params["re_record_interval"] <- list(re_record_interval)
  }
  if (!missing(log)) {
    check_bool(log, allow_null = TRUE)
    new_params["log"] <- list(log)
  }
  if (!missing(log_opts)) {
    log_opt_defaults <- list(
      file = "vcr.log",
      log_prefix = "Cassette",
      date = TRUE
    )
    new_params["log_opts"] <- list(utils::modifyList(
      log_opt_defaults,
      log_opts
    ))
  }
  if (!missing(filter_sensitive_data)) {
    check_list(filter_sensitive_data, allow_null = TRUE)
    if (length(filter_sensitive_data) > 0) {
      filter_sensitive_data <- Map(
        trimquotes,
        filter_sensitive_data,
        names(filter_sensitive_data)
      )
    }
    new_params["filter_sensitive_data"] <- list(filter_sensitive_data)
  }
  if (!missing(filter_sensitive_data_regex)) {
    check_list(filter_sensitive_data_regex, allow_null = TRUE)
    new_params["filter_sensitive_data_regex"] <- list(
      filter_sensitive_data_regex
    )
  }
  if (!missing(filter_request_headers)) {
    if (is.character(filter_request_headers)) {
      filter_request_headers <- as.list(filter_request_headers)
    }
    check_list(filter_request_headers, allow_null = TRUE)
    new_params["filter_request_headers"] <- list(filter_request_headers)
  }
  if (!missing(filter_response_headers)) {
    if (is.character(filter_response_headers)) {
      filter_response_headers <- as.list(filter_response_headers)
    }
    check_list(filter_response_headers, allow_null = TRUE)
    new_params["filter_response_headers"] <- list(filter_response_headers)
  }
  if (!missing(filter_query_parameters)) {
    if (is.character(filter_query_parameters)) {
      filter_query_parameters <- as.list(filter_query_parameters)
    }
    check_list(filter_query_parameters, allow_null = TRUE)
    lapply(filter_query_parameters, function(w) {
      if (!length(w) %in% 0:2)
        stop("filter query values must be of length 1 or 2", call. = FALSE)
    })
    new_params["filter_query_parameters"] <- list(filter_query_parameters)
  }
  if (!missing(write_disk_path)) {
    check_string(write_disk_path, allow_null = TRUE)
    new_params["write_disk_path"] <- list(write_disk_path)
  }
  if (!missing(verbose_errors)) {
    check_bool(verbose_errors, allow_null = TRUE)
    new_params["verbose_errors"] <- list(verbose_errors)
  }
  if (!missing(warn_on_empty_cassette)) {
    check_bool(warn_on_empty_cassette, allow_null = TRUE)
    new_params["warn_on_empty_cassette"] <- list(warn_on_empty_cassette)
  }

  if (length(new_params) == 0) {
    the$config
  } else {
    old_params <- the$config[names(new_params)]
    the$config[names(new_params)] <- new_params
    invisible(old_params)
  }
}

#' @export
#' @rdname vcr_configure
#' @inheritParams local_cassette
local_vcr_configure <- function(..., .frame = parent.frame()) {
  old <- vcr_configure(...)
  withr::defer(exec(vcr_configure, !!!old), envir = .frame)
  invisible()
}

#' @export
#' @rdname vcr_configure
vcr_configure_reset <- function() {
  the$config <- vcr_config_defaults()
}

#' @export
#' @rdname vcr_configure
vcr_configuration <- function() the$config

#' @export
#' @rdname vcr_configure
vcr_config_defaults <- function() {
  list(
    dir = NULL,
    record = "once",
    match_requests_on = c("method", "uri"),
    serialize_with = "yaml",
    json_pretty = FALSE,
    ignore_hosts = NULL,
    ignore_localhost = FALSE,
    ignore_request = NULL,
    preserve_exact_body_bytes = FALSE,
    turned_off = FALSE,
    re_record_interval = NULL,
    log = FALSE,
    log_opts = list(file = "vcr.log", log_prefix = "Cassette", date = TRUE),
    filter_sensitive_data = NULL,
    filter_sensitive_data_regex = NULL,
    filter_request_headers = NULL,
    filter_response_headers = NULL,
    filter_query_parameters = NULL,
    write_disk_path = NULL,
    verbose_errors = get_envvar_lgl("VCR_VERBOSE_ERRORS", FALSE),
    warn_on_empty_cassette = TRUE
  )
}

trimquotes <- function(x, y) {
  pattern <- "^\"|\"$|^'|'$"
  if (grepl(pattern, x)) {
    msg <- "filter_sensitive_data: leading & trailing quotes trimmed from '"
    warning(paste0(msg, y, "'"), call. = FALSE)
  }
  gsub(pattern, "", x)
}
