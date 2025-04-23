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
#' - `make_http_interaction()` method: [take_body()] utility
#' function is used to pull the request body out of the HTTP request
#' - `serialize_to_crul()` method: method: [webmockr::RequestSignature] and
#' [webmockr::Response] are used to build a request and response,
#' respectively, then passed to [webmockr::build_crul_response()]
#' to make a complete `crul` HTTP response object
#' @examples \dontrun{
#' library(vcr)
#' vcr_configure(dir = tempdir())
#'
#' res <- Cassette$new(name = "bob")
#' res$file()
#' res$recorded_at
#' res$recording()
#' res$eject()
#' res$should_remove_matching_existing_interactions()
#' res$storage_key()
#' res$match_requests_on
#'
#' # record all requests
#' res <- Cassette$new("foobar", record = "all")
#' res$eject()
#'
#' # cleanup
#' unlink(file.path(tempdir(), c("bob.yml", "foobar.yml")))
#'
#' library(vcr)
#' vcr_configure(dir = tempdir())
#' res <- Cassette$new(name = "jane")
#' library(crul)
#' # HttpClient$new("https://hb.opencpu.org")$get("get")
#' }
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
    #' @field http_interactions_ (list) internal use
    http_interactions_ = NULL,
    #' @field new_recorded_interactions (list) internal use
    new_recorded_interactions = NULL,
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
    #' @param record The record mode. Default: "once". In the future we'll support
    #' "once", "all", "none", "new_episodes". See [recording] for more information
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
      self$root_dir <- dir %||% config$dir
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
    },

    #' @description insert the cassette
    #' @return self
    insert = function() {
      if (!dir.exists(self$root_dir)) {
        dir_create(self$root_dir)
      }
      if (!file.exists(self$file())) {
        self$recorded_at <- Sys.time()
      } else {
        self$recorded_at <- file.mtime(self$file())
      }

      # check for re-record
      if (self$should_re_record()) self$record <- "all"

      # get previously recorded interactions
      ## if none pass, if some found, make webmockr stubs
      #### first, get previously recorded interactions into `http_interactions_` var
      self$http_interactions()
      # then do the rest

      prev <- self$previously_recorded_interactions()
      if (length(prev) > 0) {
        stub_previous_request <- function(previous_interaction) {
          req <- previous_interaction$request
          res <- previous_interaction$response
          uripp <- crul::url_parse(req$uri)
          m <- self$match_requests_on

          .stub_request_with <- function(match_parameters, request) {
            .check_match_parameters <- function(mp) {
              vmp <- c("method", "uri", "body", "headers", "query")
              mp[mp %in% vmp]
            }

            mp <- .check_match_parameters(match_parameters)

            stub_method <- ifelse("method" %in% mp, req$method, "any")

            stub_uri <- ifelse(
              identical(mp, c("body")),
              ".+",
              ifelse("uri" %in% mp, req$uri, ".")
            )

            if (stub_uri %in% c(".", ".+")) {
              sr <- webmockr::stub_request(
                method = stub_method,
                uri_regex = stub_uri
              )
            } else {
              sr <- webmockr::stub_request(method = stub_method, uri = stub_uri)
            }

            with_list <- list()

            if ("query" %in% mp) {
              with_list[["query"]] <- uripp$parameter
            }

            if ("headers" %in% mp) {
              with_list[["headers"]] <- req$headers
            }

            if ("body" %in% mp) {
              with_list[["body"]] <- req$body
            }

            # if list is empty, skip wi_th
            if (length(with_list) != 0) webmockr::wi_th(sr, .list = with_list)
          }

          .stub_request_with(m, req)
        }

        invisible(lapply(prev, stub_previous_request))
      }

      opts <- compact(list(
        name = self$name,
        record = self$record,
        serialize_with = self$serialize_with,
        match_requests_on = self$match_requests_on,
        allow_playback_repeats = self$allow_playback_repeats,
        preserve_exact_body_bytes = self$preserve_exact_body_bytes
      ))
      init_opts <- paste(names(opts), unname(opts), sep = ": ", collapse = ", ")
      vcr_log_sprintf("Initialized with options: {%s}", init_opts)

      # create new env for recorded interactions
      self$new_recorded_interactions <- list()

      # check on write to disk path
      if (!is.null(vcr_c$write_disk_path)) dir_create(vcr_c$write_disk_path)
    },

    #' @description ejects the cassette
    #' @return self
    eject = function() {
      self$write_recorded_interactions_to_disk()

      if (self$is_empty() && self$warn_on_empty) {
        cli::cli_warn(c(
          x = "{.str {self$name}} cassette ejected without recording any interactions.",
          i = "Did your request error?",
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

    #' @description Get interactions to record
    #' @return list
    merged_interactions = function() {
      old_interactions <- self$previously_recorded_interactions()

      if (self$should_remove_matching_existing_interactions()) {
        new_interaction_list <- HTTPInteractionList$new(
          self$new_recorded_interactions,
          self$match_requests_on
        )
        old_interactions <- Filter(
          function(x) {
            !unlist(new_interaction_list$has_interaction_matching(x$request))
          },
          old_interactions
        )
      }

      return(c(
        self$up_to_date_interactions(old_interactions),
        self$new_recorded_interactions
      ))
    },

    #' @description Cleans out any old interactions based on the
    #' re_record_interval and clean_outdated_http_interactions settings
    #' @param interactions list of http interactions, of class [HTTPInteraction]
    #' @return list of interactions to record
    up_to_date_interactions = function(interactions) {
      if (
        !self$clean_outdated_http_interactions &&
          is.null(self$re_record_interval)
      ) {
        return(interactions)
      }
      Filter(
        function(z) {
          as.POSIXct(z$recorded_at, tz = "GMT") >
            (as.POSIXct(Sys.time(), tz = "GMT") - self$re_record_interval)
        },
        interactions
      )
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

    #' @description Is record mode NOT "all"?
    #' @return logical
    should_stub_requests = function() {
      self$record != "all"
    },

    #' @description Is record mode "all"?
    #' @return logical
    should_remove_matching_existing_interactions = function() {
      self$record == "all"
    },

    #' @description get all previously recorded interactions
    #' @return list
    previously_recorded_interactions = function() {
      if (self$is_empty()) return(list())

      interactions <- self$serializer$deserialize()$http_interactions

      compact(lapply(interactions, function(z) {
        request <- Request$new(
          z$request$method,
          z$request$uri,
          z$request$body$string,
          z$request$headers,
          disk = z$response$body$file
        )
        if (should_be_ignored(request)) {
          return(NULL)
        }

        response <- VcrResponse$new(
          z$response$status,
          z$response$headers,
          z$response$body$string %||% z$response$body$base64_string,
          disk = z$response$body$file
        )
        HTTPInteraction$new(request = request, response = response)
      }))
    },

    #' @description write recorded interactions to disk
    #' @return nothing returned
    write_recorded_interactions_to_disk = function() {
      if (!self$any_new_recorded_interactions()) return(NULL)

      interactions <- self$merged_interactions()
      if (length(interactions) == 0) return(NULL)
      self$serializer$serialize(interactions)
    },

    #' @description record an http interaction (doesn't write to disk)
    #' @param x a crul, httr, or httr2 response object, with the request at `$request`
    #' @return nothing returned
    record_http_interaction = function(x) {
      int <- self$make_http_interaction(x)
      self$http_interactions_$response_for(int$request)
      vcr_log_sprintf(
        "Recorded HTTP interaction: %s => %s",
        request_summary(int$request),
        response_summary(int$response)
      )
      self$new_recorded_interactions <- c(self$new_recorded_interactions, int)
    },

    #' @description Are there any new recorded interactions?
    #' @return logical
    any_new_recorded_interactions = function() {
      length(self$new_recorded_interactions) != 0
    },

    #' @description make [HTTPInteractionList] object, assign to http_interactions_ var
    #' @return nothing returned
    http_interactions = function() {
      if (self$should_stub_requests()) {
        interactions <- self$previously_recorded_interactions()
      } else {
        interactions <- list()
      }

      self$http_interactions_ <- HTTPInteractionList$new(
        interactions = interactions,
        request_matchers = self$match_requests_on
      )
    },

    #' @description Make an `HTTPInteraction` object
    #' @param x A crul, httr, or httr2 response object, with the request at `$request`
    #' @return an object of class [HTTPInteraction]
    make_http_interaction = function(x) {
      # for httr2, duplicate `body` slot in `content`
      if (inherits(x, "httr2_response")) x$content <- x$body

      # content must be raw or character
      assert(unclass(x$content), c('raw', 'character'))
      new_file_path <- ""
      is_disk <- FALSE
      if (is.character(x$content)) {
        if (file.exists(x$content)) {
          is_disk <- TRUE
          write_disk_path <- vcr_c$write_disk_path
          if (is.null(write_disk_path))
            stop(
              "if writing to disk, write_disk_path must be given; ",
              "see ?vcr_configure"
            )
          new_file_path <- file.path(write_disk_path, basename(x$content))
        }
      }

      request <- Request$new(
        method = x$request$method,
        uri = x$url,
        body = take_body(x$request),
        headers = if (inherits(x, c("response", "httr2_response"))) {
          as.list(x$request$headers)
        } else {
          x$request_headers
        },
        disk = is_disk,
        skip_port_stripping = TRUE
      )

      response <- VcrResponse$new(
        status = if (inherits(x, "response")) {
          c(list(status_code = x$status_code), httr::http_status(x))
        } else if (inherits(x, "httr2_response")) {
          list(
            status_code = x$status_code,
            message = httr2::resp_status_desc(x)
          )
        } else {
          unclass(x$status_http())
        },
        headers = if (inherits(x, c("response", "httr2_response"))) {
          x$headers
        } else {
          x$response_headers
        },
        body = if (is.raw(x$content) || is.null(x$content)) {
          if (can_rawToChar(x$content)) rawToChar(x$content) else x$content
        } else {
          stopifnot(inherits(unclass(x$content), "character"))
          if (file.exists(x$content)) {
            # calculate new file path in fixtures/
            # copy file into fixtures/file_cache/
            # don't move b/c don't want to screw up first use before using
            # cached request
            file.copy(
              x$content,
              write_disk_path,
              overwrite = TRUE,
              recursive = TRUE
            ) # copy the file
            new_file_path
            # raw(0)
          } else {
            x$content
          }
        },
        disk = is_disk
      )
      HTTPInteraction$new(request = request, response = response)
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
