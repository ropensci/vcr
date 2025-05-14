#' @title Cassette handler
#' @description Main R6 class that is called from the main user facing
#' function [use_cassette()]
#' @export
#' @keywords internal
#' @return an object of class `Cassette`
#' @seealso [vcr_configure()], [use_cassette()], [insert_cassette()]
#' @section Points of webmockr integration:
#' - `initialize()`: webmockr is used in the `initialize()` method to
#'   create webmockr stubs. Stubs are created on call to `Cassette$new()`
#'   within `insert_cassette()`, but then on exiting `use_cassette()`,
#'   or calling `eject()` on `Cassette` class from `insert_cassette()`,
#'   stubs are cleaned up.
#' - `eject()` method: [webmockr::disable()] is called before exiting
#'   eject to disable webmock so that webmockr does not affect any HTTP
#'   requests that happen afterwards.
#' - `serialize_to_crul()` method: [webmockr::RequestSignature] and
#'   [webmockr::Response] are used to build a request and response,
#'   respectively, then passed to [webmockr::build_crul_response()]
#'   to make a complete `crul` HTTP response object.
Cassette <- R6::R6Class(
  "Cassette",
  public = list(
    #' @field name (character) cassette name
    name = NA,
    #' @field record (character) record mode
    record = "all",
    #' @field recorded_at (character) date/time recorded at
    recorded_at = NA,
    #' @field serialize_with (character) serializer (yaml|json|qs2)
    serialize_with = "yaml",
    #' @field serializer (Serializer) serializer (YAML|JSON|QS2)
    serializer = NA,
    #' @field match_requests_on (character) matchers to use
    #' default: method & uri
    match_requests_on = c("method", "uri"),
    #' @field re_record_interval (numeric) the re-record interval
    re_record_interval = NULL,
    #' @field root_dir root dir, gathered from [vcr_configuration()]
    root_dir = NA,
    #' @field preserve_exact_body_bytes (logical) Whether to base64 encode the
    #' bytes of the requests and responses
    preserve_exact_body_bytes = FALSE,
    #' @field http_interactions (list) internal use
    http_interactions = NULL,
    #' @field new_interactions (boolean) Have any interactions been recorded?
    new_interactions = FALSE,
    #' @field clean_outdated_http_interactions (logical) Should outdated interactions
    #' be recorded back to file
    clean_outdated_http_interactions = FALSE,
    #' @field to_return (logical) internal use
    to_return = NULL,
    #' @field warn_on_empty (logical) warn if no interactions recorded
    warn_on_empty = TRUE,
    #' @field new_cassette is this a new cassette?
    new_cassette = TRUE,

    #' @description Create a new `Cassette` object
    #' @param dir The directory where the cassette will be stored.
    #' @param name The name of the cassette. vcr will sanitize this to ensure it
    #' is a valid file name.
    #' @param record The record mode. Default: "once".
    #' @param serialize_with (character) Which serializer to use.
    #' Valid values are "yaml" (default), "json", and "qs2".
    #' @param match_requests_on List of request matchers
    #' to use to determine what recorded HTTP interaction to replay. Defaults to
    #' `["method", "uri"]`. The built-in matchers are "method", "uri",
    #' "headers" and "body" ("host" and "path" not supported yet, but should
    #' be in a future version)
    #' @param re_record_interval (numeric) When given, the cassette will be
    #' re-recorded at the given interval, in seconds.
    #' @param preserve_exact_body_bytes (logical) Whether or not
    #' to base64 encode the bytes of the requests and responses for
    #' this cassette when serializing it. See also `preserve_exact_body_bytes`
    #' in [vcr_configure()]. Default: `FALSE`
    #' @param clean_outdated_http_interactions (logical) Should outdated interactions
    #' be recorded back to file. Default: `FALSE`
    #' @param warn_on_empty Warn when ejecting the cassette if no interactions
    #'   have been recorded.
    #' @return A new `Cassette` object
    initialize = function(
      name,
      dir = NULL,
      record = NULL,
      match_requests_on = NULL,
      serialize_with = NULL,
      preserve_exact_body_bytes = NULL,
      re_record_interval = NULL,
      clean_outdated_http_interactions = NULL,
      warn_on_empty = NULL
    ) {
      check_cassette_name(name)

      self$name <- name
      self$root_dir <- dir %||% the$config$dir %||% testthat::test_path("_vcr")
      self$record <- check_record_mode(record) %||% the$config$record
      self$match_requests_on <- check_request_matchers(match_requests_on) %||%
        the$config$match_requests_on
      self$serialize_with <- serialize_with %||% the$config$serialize_with
      self$re_record_interval <- re_record_interval %||%
        the$config$re_record_interval

      assert(preserve_exact_body_bytes, "logical")
      self$preserve_exact_body_bytes <- preserve_exact_body_bytes %||%
        the$config$preserve_exact_body_bytes

      self$clean_outdated_http_interactions <- clean_outdated_http_interactions %||%
        the$config$clean_outdated_http_interactions

      self$warn_on_empty <- warn_on_empty %||% the$config$warn_on_empty_cassette

      self$serializer <- serializer_fetch(
        self$serialize_with,
        path = self$root_dir,
        name = self$name,
        preserve_bytes = self$preserve_exact_body_bytes,
        matchers = self$match_requests_on
      )

      if (!file.exists(self$file())) {
        self$recorded_at <- Sys.time()
      } else {
        self$recorded_at <- file.mtime(self$file())
      }

      # check for re-record
      if (self$should_re_record()) self$record <- "all"
    },

    #' @description insert the cassette
    #' @return self
    insert = function() {
      name <- basename(self$file())

      if (!file.exists(self$file()) || self$record == "all") {
        vcr_log_sprintf("Inserting '%s' (new cassette)", name)
        self$new_cassette <- TRUE
        interactions <- list()
      } else {
        self$new_cassette <- FALSE
        interactions <- self$serializer$deserialize()$http_interactions
        n <- length(interactions)
        vcr_log_sprintf("Inserting '%s' (with %d interactions)", name, n)

        interactions <- Filter(\(x) !should_be_ignored(x$request), interactions)
        if (self$clean_outdated_http_interactions) {
          if (!is.null(self$re_record_interval)) {
            threshold <- Sys.time() - self$re_record_interval
            interactions <- Filter(\(x) x$recorded_at > threshold, interactions)
          }
        }

        m <- length(interactions)
        if (m < n) {
          vcr_log_sprintf("Filtering: removed %d interactions", n - m)
        }
      }

      self$http_interactions <- Interactions$new(
        interactions = interactions,
        request_matchers = self$match_requests_on,
        replayable = self$record != "all"
      )

      vcr_log_sprintf("  recording: %s", self$recording())
    },

    #' @description ejects the cassette
    #' @return self
    eject = function() {
      vcr_log_sprintf("Ejecting")

      if (self$http_interactions$length() == 0 && self$warn_on_empty) {
        cli::cli_warn(c(
          x = "{.str {self$name}} cassette ejected without recording any interactions.",
          i = "Did you use {{curl}}, `download.file()`, or other unsupported tool?",
          i = "If you are using crul/httr/httr2, are you sure you made an HTTP request?"
        ))
      }
      invisible(self)
    },

    #' @description print method for `Cassette` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat(paste0("<vcr - Cassette> ", self$name), sep = "\n")
      cat(paste0("  Record method: ", self$record), sep = "\n")
      cat(paste0("  Serialize with: ", self$serialize_with), sep = "\n")
      cat(
        paste0("  Re-record interval (s): ", self$re_record_interval),
        sep = "\n"
      )
      cat(
        paste0(
          "  Clean outdated interactions?: ",
          self$clean_outdated_http_interactions
        ),
        sep = "\n"
      )
      cat(
        paste0("  preserve_exact_body_bytes: ", self$preserve_exact_body_bytes),
        sep = "\n"
      )
      invisible(self)
    },

    #' @description get the file path for the cassette
    #' @return character
    file = function() self$serializer$path,

    #' @description is the cassette in recording mode?
    #' @return logical
    recording = function() {
      if (self$record == "none") {
        FALSE
      } else if (self$record == "once") {
        self$new_cassette
      } else {
        TRUE
      }
    },

    #' @description Should re-record interactions?
    #' @return logical
    should_re_record = function() {
      if (is.null(self$re_record_interval)) return(FALSE)
      now <- as.POSIXct(Sys.time(), tz = "GMT")
      time_comp <- (self$recorded_at + self$re_record_interval) < now
      info <- sprintf(
        "previously recorded at: '%s'; now: '%s'; interval: %s seconds",
        self$recorded_at,
        now,
        self$re_record_interval
      )

      if (!time_comp) {
        vcr_log_sprintf(
          "Not re-recording since the interval has not elapsed (%s).",
          info
        )
        return(FALSE)
      } else if (curl::has_internet()) {
        vcr_log_sprintf("re-recording (%s).", info)
        return(TRUE)
      } else {
        vcr_log_sprintf(
          "Not re-recording because no internet connection is available (%s).",
          info
        )
        return(FALSE)
      }
    },

    #' @description record an http interaction (doesn't write to disk)
    #' @param request A `vcr_request`.
    #' @param response A `vcr_response`.
    #' @return an interaction as a list with request and response slots
    record_http_interaction = function(request, response) {
      vcr_log_sprintf("  recording response: %s", response_summary(response))

      self$new_interactions <- TRUE
      self$http_interactions$add(request, response)

      dir_create(self$root_dir)
      self$serializer$serialize(self$http_interactions$interactions)
    }
  )
)
