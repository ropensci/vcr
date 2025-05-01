#' @title Cassette handler
#' @description Main R6 class that is called from the main user facing
#' function [use_cassette()]
#' @export
#' @keywords internal
#' @return an object of class `Cassette`
#' @seealso [vcr_configure()], [use_cassette()], [insert_cassette()]
#' @section Points of webmockr integration:
#' - `initialize()`: webmockr is used in the `initialize()` method to
#' create webmockr stubs. stubs are created on call to `Cassette$new()`
#' within `insert_cassette()`, but then on exiting `use_cassette()`,
#' or calling `eject()` on `Cassette` class from `insert_cassette()`,
#' stubs are cleaned up.
#' - `eject()` method: [webmockr::disable()] is called before exiting
#' eject to disable webmock so that webmockr does not affect any HTTP
#' requests that happen afterwards
#' - `serialize_to_crul()` method: method: [webmockr::RequestSignature] and
#' [webmockr::Response] are used to build a request and response,
#' respectively, then passed to [webmockr::build_crul_response()]
#' to make a complete `crul` HTTP response object
Cassette <- R6::R6Class(
  "Cassette",
  public = list(
    #' @field name (character) cassette name
    name = NA,
    #' @field record (character) record mode
    record = "all",
    #' @field recorded_at (character) date/time recorded at
    recorded_at = NA,
    #' @field serialize_with (character) serializer to use (yaml|json)
    serialize_with = "yaml",
    #' @field serializer (character) serializer to use (yaml|json)
    serializer = NA,
    #' @field match_requests_on (character) matchers to use
    #' default: method & uri
    match_requests_on = c("method", "uri"),
    #' @field re_record_interval (numeric) the re-record interval
    re_record_interval = NULL,
    #' @field root_dir root dir, gathered from [vcr_configuration()]
    root_dir = NA,
    #' @field allow_playback_repeats (logical) Whether to allow a single HTTP
    #' interaction to be played back multiple times
    allow_playback_repeats = FALSE,
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

    #' @description Create a new `Cassette` object
    #' @param dir The directory where the cassette will be stored.
    #' @param name The name of the cassette. vcr will sanitize this to ensure it
    #' is a valid file name.
    #' @param record The record mode. Default: "once".
    #' @param serialize_with (character) Which serializer to use.
    #'  Valid values are "yaml" (default), the only one supported for now.
    #' @param match_requests_on List of request matchers
    #' to use to determine what recorded HTTP interaction to replay. Defaults to
    #' `["method", "uri"]`. The built-in matchers are "method", "uri",
    #' "headers" and "body" ("host" and "path" not supported yet, but should
    #' be in a future version)
    #' @param re_record_interval (numeric) When given, the cassette will be
    #' re-recorded at the given interval, in seconds.
    #' @param allow_playback_repeats (logical) Whether or not to
    #' allow a single HTTP interaction to be played back multiple times.
    #' Default: `FALSE`.
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
      allow_playback_repeats = FALSE,
      serialize_with = NULL,
      preserve_exact_body_bytes = NULL,
      re_record_interval = NULL,
      clean_outdated_http_interactions = NULL,
      warn_on_empty = NULL
    ) {
      check_cassette_name(name)
      config <- vcr_configuration()

      self$name <- name
      self$root_dir <- dir %||% config$dir %||% testthat::test_path("_vcr")
      self$record <- check_record_mode(record %||% config$record)
      self$match_requests_on <- check_request_matchers(match_requests_on) %||%
        config$match_requests_on
      self$serialize_with <- serialize_with %||% config$serialize_with
      self$re_record_interval <- re_record_interval %||%
        config$re_record_interval
      self$allow_playback_repeats = allow_playback_repeats

      assert(preserve_exact_body_bytes, "logical")
      self$preserve_exact_body_bytes <- preserve_exact_body_bytes %||%
        config$preserve_exact_body_bytes

      self$clean_outdated_http_interactions <- clean_outdated_http_interactions %||%
        config$clean_outdated_http_interactions

      self$warn_on_empty <- warn_on_empty %||% config$warn_on_empty_cassette

      self$serializer <- serializer_fetch(
        self$serialize_with,
        path = self$root_dir,
        name = self$name,
        preserve_bytes = self$preserve_exact_body_bytes
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
      if (self$is_empty()) {
        interactions <- list()
      } else {
        interactions <- self$serializer$deserialize()$http_interactions
        interactions <- Filter(\(x) !should_be_ignored(x$request), interactions)

        if (self$clean_outdated_http_interactions) {
          if (!is.null(self$re_record_interval)) {
            threshold <- Sys.time() - self$re_record_interval
            interactions <- Filter(\(x) x$recorded_at > threshold, interactions)
          }
        }
      }
      vcr_log_sprintf(
        "Inserting: loading %d interactions from disk",
        length(interactions)
      )

      self$http_interactions <- HTTPInteractionList$new(
        interactions = interactions,
        request_matchers = self$match_requests_on,
        replayable = self$record != "all"
      )

      vcr_log_sprintf("  record: %s", self$record)
      vcr_log_sprintf("  serialize_with: %s", self$serialize_with)
      vcr_log_sprintf(
        "  allow_playback_repeats: %s",
        self$allow_playback_repeats
      )
      vcr_log_sprintf(
        "  preserve_exact_body_bytes: %s",
        self$preserve_exact_body_bytes
      )
    },

    #' @description ejects the cassette
    #' @return self
    eject = function() {
      if (self$new_interactions) {
        dir_create(self$root_dir)

        interactions <- self$http_interactions$interactions
        self$serializer$serialize(interactions)
        vcr_log_sprintf(
          "Ejecting: writing %i interactions",
          length(interactions)
        )
      } else {
        vcr_log_sprintf("Ejecting")
      }

      if (self$is_empty() && self$warn_on_empty) {
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
        paste0("  allow_playback_repeats: ", self$allow_playback_repeats),
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
        return(FALSE)
      } else if (self$record == "once") {
        return(self$is_empty())
      } else {
        return(TRUE)
      }
    },

    #' @description is the cassette on disk empty
    #' @return logical
    is_empty = function() {
      !file.exists(self$file())
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
    }
  )
)

check_cassette_name <- function(x, call = caller_env()) {
  if (length(x) != 1 || !is.character(x)) {
    cli::cli_abort("{.arg name} must be a single string.", call = call)
  }

  if (any(x %in% cassette_names())) {
    cli::cli_abort(
      "{.arg name} must not be the same as an existing cassette.",
      call = call
    )
  }

  if (grepl("\\s", x)) {
    cli::cli_abort("{.arg name} must not contain spaces.", call = call)
  }

  if (grepl("\\.yml$|\\.yaml$", x)) {
    cli::cli_abort("{.arg name} must not include an extension.", call = call)
  }

  # the below adapted from fs::path_sanitize, which adapted
  # from the npm package sanitize-filename
  illegal <- "[/\\?<>\\:*|\":]"
  control <- "[[:cntrl:]]"
  reserved <- "^[.]+$"
  windows_reserved <- "^(con|prn|aux|nul|com[0-9]|lpt[0-9])([.].*)?$"
  windows_trailing <- "[. ]+$"
  if (grepl(illegal, x))
    cli::cli_abort(
      "{.arg name} must not contain '/', '?', '<', '>', '\\', ':', '*', '|', or '\"'",
      call = call
    )
  if (grepl(control, x)) {
    cli::cli_abort(
      "{.arg name} must not contain control characters.",
      call = call
    )
  }
  if (grepl(reserved, x)) {
    cli::cli_abort(
      "{.arg name} must not be '.', '..', etc.",
      call = call
    )
  }
  if (grepl(windows_reserved, x)) {
    cli::cli_abort(
      "{.arg name} must not contain reserved windows strings.",
      call = call
    )
  }
  if (grepl(windows_trailing, x)) {
    cli::cli_abort("{.arg name} must not end in '.'.", call = call)
  }
  if (nchar(x) > 255) {
    cli::cli_abort("{.arg name} must be less than 256 characters.", call = call)
  }

  invisible()
}
