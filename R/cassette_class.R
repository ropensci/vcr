#' @title Cassette handler
#' @description Main R6 class that is called from the main user facing
#' function [use_cassette()]
#' @export
#' @keywords internal
#' @return An R6 `Cassette` pbject.
#' @seealso [vcr_configure()], [use_cassette()], [insert_cassette()]
Cassette <- R6::R6Class(
  "Cassette",
  public = list(
    #' @field name (character) cassette name
    name = NA,
    #' @field record (character) record mode
    record = "all",
    #' @field serialize_with (character) serializer (yaml|json|qs2)
    serialize_with = "yaml",
    #' @field serializer (Serializer) serializer (YAML|JSON|QS2)
    serializer = NA,
    #' @field match_requests_on (character) matchers to use
    match_requests_on = "default",
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
    #' @param match_requests_on HTTP request components to use when matching.
    #' @param re_record_interval (numeric) When given, the cassette will be
    #' re-recorded at the given interval, in seconds.
    #' @param preserve_exact_body_bytes (logical) Whether or not
    #' to base64 encode the bytes of the requests and responses for
    #' this cassette when serializing it. See also `preserve_exact_body_bytes`
    #' in [vcr_configure()]. Default: `FALSE`
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

      check_bool(preserve_exact_body_bytes, allow_null = TRUE)
      self$preserve_exact_body_bytes <- preserve_exact_body_bytes %||%
        the$config$preserve_exact_body_bytes

      self$warn_on_empty <- warn_on_empty %||% the$config$warn_on_empty_cassette

      self$serializer <- serializer_fetch(
        self$serialize_with,
        path = self$root_dir,
        name = self$name,
        preserve_bytes = self$preserve_exact_body_bytes,
        matchers = self$match_requests_on
      )
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
      }

      self$http_interactions <- Interactions$new(
        interactions = interactions,
        request_matchers = self$match_requests_on
      )
      self$remove_outdated_interactions()

      private$old_env_var <- set_env_var(list(
        VCR_IS_RECORDING = self$recording(),
        VCR_IS_REPLAYING = self$replaying()
      ))

      if (self$recording() && self$replaying()) {
        vcr_log_sprintf("  Mode: recording and replaying")
      } else if (self$recording()) {
        vcr_log_sprintf("  Mode: recording")
      } else if (self$replaying()) {
        vcr_log_sprintf("  Mode: replaying")
      } else {
        vcr_log_sprintf("  Mode: disabled")
      }
    },

    #' @description ejects the cassette
    #' @return self
    eject = function() {
      vcr_log_sprintf("Ejecting")

      set_env_var(private$old_env_var)

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
      cat_line("<vcr - Cassette> ", self$name)
      cat_line("  Record method: ", self$record)
      cat_line("  Serialize with: ", self$serialize_with)
      if (!is.null(self$re_record_interval)) {
        cat_line("  Re-record interval (s): ", self$re_record_interval)
      }
      cat_line("  preserve_exact_body_bytes: ", self$preserve_exact_body_bytes)
      invisible(self)
    },

    #' @description get the file path for the cassette
    #' @return character
    file = function() self$serializer$path,

    #' @description Is the cassette in recording mode?
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

    #' @description Is the cassette in replaying mode?
    #' @return logical
    replaying = function() {
      self$http_interactions$n_replayable() > 0
    },

    #' @description Remove outdated interactions
    remove_outdated_interactions = function() {
      if (is.null(self$re_record_interval)) {
        return()
      }

      threshold <- Sys.time() - self$re_record_interval
      outdated <- vapply(
        self$http_interactions$interactions,
        \(x) x$recorded_at < threshold,
        logical(1)
      )

      if (!any(outdated)) {
        return()
      }
      self$record <- "new_episodes"
      vcr_log_sprintf(
        "Removing %i outdated interactions and re-recording.",
        sum(outdated)
      )
      self$http_interactions$interactions <- self$http_interactions$interactions[
        !outdated
      ]
    },

    #' @description record an http interaction (doesn't write to disk)
    #' @param request A `vcr_request`.
    #' @param response A `vcr_response`.
    #' @return an interaction as a list with request and response slots
    record_http_interaction = function(request, response) {
      vcr_log_sprintf("  Recording response: %s", response_summary(response))

      self$new_interactions <- TRUE
      self$http_interactions$add(request, response)

      dir_create(self$root_dir)
      self$serializer$serialize(self$http_interactions$interactions)
    }
  ),
  private = list(
    old_env_var = character()
  )
)
