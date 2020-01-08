#' @title Cassette handler
#' @description Main R6 class that is called from the main user facing
#' function [use_cassette()]
#' @export
#' @keywords internal
#' @return an object of class `Cassette`
#' @seealso [vcr_configure()], [use_cassette()], [insert_cassette()]
#' @examples
#' library(vcr)
#' vcr_configure(dir = tempdir())
#'
#' res <- Cassette$new(name = "bob")
#' res$file()
#' res$originally_recorded_at()
#' res$recording()
#' res$serializable_hash()
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
Cassette <- R6::R6Class(
  "Cassette",
  public = list(
    #' @field name (character) cassette name
    name = NA,
    #' @field record (character) record mode
    record = "all",
    #' @field manfile (character) cassette file path
    manfile = NA,
    #' @field recorded_at (character) date/time recorded at
    recorded_at = NA,
    #' @field serialize_with (character) serializer to use (yaml only)
    serialize_with = "yaml",
    #' @field serializer (character) serializer to use (yaml only)
    serializer = NA,
    #' @field persist_with (character) persister to use (FileSystem only)
    persist_with = "FileSystem",
    #' @field persister (character) persister to use (FileSystem only)
    persister = NA,
    #' @field match_requests_on (character) matchers to use
    #' default: method & uri
    match_requests_on = c("method", "uri"),
    #' @field re_record_interval (numeric) the re-record interval
    re_record_interval = NULL,
    #' @field tag ignored, not used right now
    tag = NA,
    #' @field tags ignored, not used right now
    tags = NA,
    #' @field root_dir root dir, gathered from [vcr_configuration()]
    root_dir = NA,
    #' @field update_content_length_header (logical) Whether to overwrite the
    #' `Content-Length` header
    update_content_length_header = FALSE,
    #' @field decode_compressed_response (logical) ignored, not used right now
    decode_compressed_response = FALSE,
    #' @field allow_playback_repeats (logical) Whether to allow a single HTTP
    #' interaction to be played back multiple times
    allow_playback_repeats = FALSE,
    #' @field allow_unused_http_interactions (logical) ignored, not used right now
    allow_unused_http_interactions = TRUE,
    #' @field exclusive (logical) ignored, not used right now
    exclusive = FALSE,
    #' @field preserve_exact_body_bytes (logical) Whether to base64 encode the
    #' bytes of the requests and responses
    preserve_exact_body_bytes = FALSE,
    #' @field args (list) internal use
    args = list(),
    #' @field http_interactions_ (list) internal use
    http_interactions_ = NULL,
    #' @field new_recorded_interactions (list) internal use
    new_recorded_interactions = NULL,
    #' @field clean_outdated_http_interactions (logical) Should outdated interactions
    #' be recorded back to file
    clean_outdated_http_interactions = FALSE,
    #' @field to_return (logical) internal use
    to_return = NULL,
    #' @field cassette_opts (list) various cassette options
    cassette_opts = NULL,

    #' @description Create a new `Cassette` object
    #' @param name The name of the cassette. vcr will sanitize this to ensure it
    #' is a valid file name.
    #' @param record The record mode. Default: "once". In the future we'll support
    #' "once", "all", "none", "new_episodes". See [recording] for more information
    #' @param serialize_with (character) Which serializer to use.
    #'  Valid values are "yaml" (default), the only one supported for now.
    #' @param persist_with (character) Which cassette persister to
    #'  use. Default: "file_system". You can also register and use a
    #'  custom persister.
    #' @param match_requests_on List of request matchers
    #' to use to determine what recorded HTTP interaction to replay. Defaults to
    #' `["method", "uri"]`. The built-in matchers are "method", "uri",
    #' "headers" and "body" ("host" and "path" not supported yet, but should
    #' be in a future version)
    #' @param re_record_interval (numeric) When given, the cassette will be
    #' re-recorded at the given interval, in seconds.
    #' @param tag,tags tags ignored, not used right now
    #' @param update_content_length_header (logical) Whether or
    #' not to overwrite the `Content-Length` header of the responses to
    #' match the length of the response body. Default: `FALSE`
    #' @param decode_compressed_response (logical) ignored, not used right now
    #' @param allow_playback_repeats (logical) Whether or not to
    #' allow a single HTTP interaction to be played back multiple times.
    #' Default: `FALSE`.
    #' @param allow_unused_http_interactions (logical) ignored, not used right now
    #' @param exclusive (logical) ignored, not used right now
    #' @param preserve_exact_body_bytes (logical) Whether or not
    #' to base64 encode the bytes of the requests and responses for
    #' this cassette when serializing it. See also `preserve_exact_body_bytes`
    #' in [vcr_configure()]. Default: `FALSE`
    #' @param clean_outdated_http_interactions (logical) Should outdated interactions
    #' be recorded back to file. Default: `FALSE`
    #' @return A new `Cassette` object
    initialize = function(
      name, record, serialize_with = "yaml",
      persist_with = "FileSystem",
      match_requests_on = c("method", "uri"),
      re_record_interval, tag, tags, update_content_length_header,
      decode_compressed_response, allow_playback_repeats,
      allow_unused_http_interactions, exclusive, preserve_exact_body_bytes,
      clean_outdated_http_interactions) {

      self$name <- name
      self$root_dir <- vcr_configuration()$dir
      self$serialize_with <- serialize_with
      self$persist_with <- persist_with
      if (!missing(record)) {
        self$record <- check_record_mode(record)
      }
      self$make_dir()
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()),
                              self$name)
      if (!file.exists(self$manfile)) cat("\n", file = self$manfile)
      if (!missing(match_requests_on)) {
        self$match_requests_on <- check_request_matchers(match_requests_on)
      }
      if (!missing(re_record_interval))
        self$re_record_interval <- re_record_interval
      if (!missing(tag)) self$tag = tag
      if (!missing(tags)) self$tags = tags
      if (!missing(update_content_length_header)) {
        assert(update_content_length_header, "logical")
        self$update_content_length_header = update_content_length_header
      }
      if (!missing(decode_compressed_response))
        self$decode_compressed_response = decode_compressed_response
      if (!missing(allow_playback_repeats)) {
        assert(allow_playback_repeats, "logical")
        self$allow_playback_repeats = allow_playback_repeats
      }
      if (!missing(allow_unused_http_interactions))
        self$allow_unused_http_interactions = allow_unused_http_interactions
      if (!missing(exclusive)) self$exclusive = exclusive
      if (!missing(preserve_exact_body_bytes)) {
        assert(preserve_exact_body_bytes, "logical")
        self$preserve_exact_body_bytes <- preserve_exact_body_bytes
      }
      if (!missing(clean_outdated_http_interactions)) {
        self$clean_outdated_http_interactions <- clean_outdated_http_interactions
      }
      self$make_args()
      if (!file.exists(self$manfile)) self$write_metadata()
      self$recorded_at <- file.info(self$file())$mtime
      self$serializer = serializer_fetch(self$serialize_with, self$name)
      self$persister = persister_fetch(self$persist_with, self$serializer$path)

      # check for re-record
      if (self$should_re_record()) self$record <- "all"

      # get previously recorded interactions
      ## if none pass, if some found, make webmockr stubs
      #### first, get previously recorded interactions into `http_interactions_` var
      self$http_interactions()
      # then do the rest
      prev <- self$previously_recorded_interactions()
      if (length(prev) > 0) {
        invisible(lapply(prev, function(z) {
          req <- z$request
          res <- z$response
          uripp <- crul::url_parse(req$uri)
          m <- self$match_requests_on
          if (length(m) == 1) {
            if (m == "method") webmockr::stub_request(req$method, uri_regex = ".")
            if (m == "uri") webmockr::stub_request("any", req$uri)
            if (m == "query") {
              tmp <- webmockr::stub_request("any", uri_regex = ".")
              webmockr::wi_th(tmp, .list = list(query = uripp$parameter))
            }
            if (m == "headers") {
              tmp <- webmockr::stub_request("any", uri_regex = ".")
              webmockr::wi_th(tmp, .list = list(headers = req$headers))
            }
            if (m == "body") {
              tmp <- webmockr::stub_request("any", uri_regex = ".+")
              webmockr::wi_th(tmp, .list = list(body = req$body))
            }
          } else if (all(m %in% c("method", "uri")) && length(m) == 2) {
            webmockr::stub_request(req$method, req$uri)
          } else if (
            all(m %in% c("method", "body")) && length(m) == 2
          ) {
            tmp <- webmockr::stub_request(req$method, uri_regex = ".")
            webmockr::wi_th(tmp, .list = list(body = req$body))
          } else if (
            all(m %in% c("method", "uri", "query")) && length(m) == 3
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter))
          } else if (
            all(m %in% c("method", "uri", "headers")) && length(m) == 3
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(headers = req$headers))
          } else if (
            all(m %in% c("method", "uri", "body")) && length(m) == 3
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(body = req$body))
          } else if (
            all(m %in% c("method", "uri", "headers", "query")) &&
            length(m) == 4
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter,
              headers = req$headers))
          } else if (
            all(m %in% c("method", "uri", "headers", "body")) &&
            length(m) == 4
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(body = req$body,
              headers = req$headers))
          } else if (
            all(m %in% c("method", "uri", "query", "body")) &&
            length(m) == 4
          ) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter,
              body = req$body))
          } else {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter,
              body = req$body, headers = req$headers))
          }

        }))
      }

      tmp <- list(
        self$name,
        self$record,
        self$serialize_with,
        self$persist_with,
        self$match_requests_on,
        self$update_content_length_header,
        self$allow_playback_repeats,
        self$preserve_exact_body_bytes
      )
      init_opts <- compact(
        stats::setNames(tmp, c("name", "record", "serialize_with",
        "persist_with", "match_requests_on", "update_content_length_header",
        "allow_playback_repeats", "preserve_exact_body_bytes")))
      self$cassette_opts <- init_opts
      init_opts <- paste(names(init_opts), unname(init_opts), sep = ": ",
        collapse = ", ")
      vcr_log_info(sprintf("Initialized with options: {%s}", init_opts),
        vcr_c$log_opts$date)

      # create new env for recorded interactions
      self$new_recorded_interactions <- list()

      # check on write to disk path
      if (!is.null(vcr_c$write_disk_path))
        dir.create(vcr_c$write_disk_path, showWarnings = FALSE, recursive = TRUE)

      # put cassette in vcr_cassettes environment
      include_cassette(self)
    },

    #' @description print method for `Cassette` objects
    #' @param x self
    #' @param ... ignored
    print = function(x, ...) {
      cat(paste0("<vcr - Cassette> ", self$name), sep = "\n")
      cat(paste0("  Record method: ", self$record), sep = "\n")
      cat(paste0("  Serialize with: ", self$serialize_with), sep = "\n")
      cat(paste0("  Persist with: ", self$persist_with), sep = "\n")
      cat(paste0("  Re-record interval (s): ", self$re_record_interval),
        sep = "\n")
      cat(paste0("  Clean outdated interactions?: ",
        self$clean_outdated_http_interactions), sep = "\n")
      cat(paste0("  update_content_length_header: ",
                 self$update_content_length_header), sep = "\n")
      cat(paste0("  decode_compressed_response: ",
                 self$decode_compressed_response), sep = "\n")
      cat(paste0("  allow_playback_repeats: ",
                 self$allow_playback_repeats), sep = "\n")
      cat(paste0("  allow_unused_http_interactions: ",
                 self$allow_unused_http_interactions), sep = "\n")
      cat(paste0("  exclusive: ", self$exclusive), sep = "\n")
      cat(paste0("  preserve_exact_body_bytes: ",
                 self$preserve_exact_body_bytes), sep = "\n")
      invisible(self)
    },

    #' @description run code
    #' @param ... pass in things to be lazy eval'ed
    #' @return various
    call_block = function(...) {
      # capture block
      tmp <- lazyeval::lazy_dots(...)
      # check if block is empty
      if (length(tmp) == 0) {
        stop("`vcr::use_cassette` requires a code block. ",
             "If you cannot wrap your code in a block, use ",
             "`vcr::insert_cassette` / `vcr::eject_cassette` instead")
      }
      # allow http interactions - disallow at end of call_block() below
      webmockr::webmockr_allow_net_connect()
      # evaluate request
      resp <- lazyeval::lazy_eval(tmp)
      # disallow http interactions - allow at start of call_block() above
      webmockr::webmockr_disable_net_connect()
    },

    #' @description ejects the current cassette
    #' @return self
    eject = function() {
      self$write_recorded_interactions_to_disk()
      # remove cassette from list of current cassettes
      rm(list = self$name, envir = vcr_cassettes)
      message("ejecting cassette: ", self$name)
      # disable webmockr
      webmockr::disable()
      # set current casette name to NULL
      vcr__env$current_cassette <- NULL
      # return self
      return(self)
    },

    #' @description get the file path for the cassette
    #' @return character
    file = function() self$manfile,

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
      nchar(self$raw_cassette_bytes()) < 1
    },

    #' @description timestamp the cassette was originally recorded at
    #' @return POSIXct date
    originally_recorded_at = function() {
      as.POSIXct(self$recorded_at, tz = "GMT")
    },

    #' @description Get a list of the http interactions to record + recorded_with
    #' @return list
    serializable_hash = function() {
      list(
        http_interactions = self$interactions_to_record(),
        recorded_with = utils::packageVersion("vcr")
      )
    },

    #' @description Get the list of http interactions to record
    #' @return list
    interactions_to_record = function() {
      ## FIXME - gotta sort out defining and using hooks better
      ## just returning exact same input
      self$merged_interactions()

      # FIXME: not sure what's going on here, so not using yet
      #.       maybe we don't need this?
      # "We dee-dupe the interactions by roundtripping them to/from a hash.
      # This is necessary because `before_record` can mutate the interactions."
      # lapply(self$merged_interactions(), function(z) {
      #   VCRHooks$invoke_hook("before_record", z)
      # })
    },

    #' @description Get interactions to record
    #' @return list
    merged_interactions = function() {
      old_interactions <- self$previously_recorded_interactions()
      old_interactions <- lapply(old_interactions, function(x) {
        HTTPInteraction$new(
          request = x$request,
          response = x$response,
          recorded_at = x$recorded_at)
      })

      if (self$should_remove_matching_existing_interactions()) {
        new_interaction_list <-
          HTTPInteractionList$new(self$new_recorded_interactions,
                                  self$match_requests_on)
        old_interactions <-
          Filter(function(x) {
              req <- Request$new()$from_hash(x$request)
              !unlist(new_interaction_list$has_interaction_matching(req))
            },
            old_interactions
          )
      }

      return(c(self$up_to_date_interactions(old_interactions),
        self$new_recorded_interactions))
    },

    #' @description Cleans out any old interactions based on the
    #' re_record_interval and clean_outdated_http_interactions settings
    #' @param interactions list of http interactions, of class [HTTPInteraction]
    #' @return list of interactions to record
    up_to_date_interactions = function(interactions) {
      if (
        !self$clean_outdated_http_interactions && is.null(self$re_record_interval)
      ) {
        return(interactions)
      }
      Filter(function(z) {
        as.POSIXct(z$recorded_at, tz = "GMT") > (as.POSIXct(Sys.time(), tz = "GMT") - self$re_record_interval)
      }, interactions)
    },

    #' @description Should re-record interactions?
    #' @return logical
    should_re_record = function() {
      if (is.null(self$re_record_interval)) return(FALSE)
      if (is.null(self$originally_recorded_at())) return(FALSE)
      now <- as.POSIXct(Sys.time(), tz = "GMT")
      time_comp <- (self$originally_recorded_at() + self$re_record_interval) < now
      info <- sprintf(
        "previously recorded at: '%s'; now: '%s'; interval: %s seconds",
        self$originally_recorded_at(), now, self$re_record_interval)

      if (!time_comp) {
        vcr_log_info(
          sprintf("Not re-recording since the interval has not elapsed (%s).", info),
          vcr_c$log_opts$date)
        return(FALSE)
      } else if (has_internet()) {
        vcr_log_info(sprintf("re-recording (%s).", info), vcr_c$log_opts$date)
        return(TRUE)
      } else {
        vcr_log_info(
          sprintf("Not re-recording because no internet connection is available (%s).", info),
          vcr_c$log_opts$date)
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
    #' @description Get the serializer path
    #' @return character
    storage_key = function() self$serializer$path,
    #' @description Get character string of entire cassette; bytes is a misnomer
    #' @return character
    raw_cassette_bytes = function() {
      file <- self$file()
      if (is.null(file)) return("")
      tmp <- readLines(file) %||% ""
      paste0(tmp, collapse = "")
    },

    #' @description Create the directory that holds the cassettes, if not present
    #' @return no return; creates a directory recursively, if missing
    make_dir = function() {
      dir.create(path.expand(self$root_dir), showWarnings = FALSE,
                 recursive = TRUE)
    },

    #' @description get http interactions from the cassette via the serializer
    #' @return list
    deserialized_hash = function() {
      tmp <- self$serializer$deserialize_path()
      if (inherits(tmp, "list")) {
        return(tmp)
      } else {
        stop(tmp, " does not appear to be a valid cassette", call. = FALSE)
      }
    },

    #' @description get all previously recorded interactions
    #' @return list
    previously_recorded_interactions = function() {
      if (nchar(self$raw_cassette_bytes()) > 0) {
        tmp <- compact(
          lapply(self$deserialized_hash()[["http_interactions"]], function(z) {
          response <- VcrResponse$new(
            z$response$status,
            z$response$headers,
            z$response$body$string,
            opts = self$cassette_opts,
            disk = z$response$body$file
          )
          if (self$update_content_length_header)
            response$update_content_length_header()
          zz <- HTTPInteraction$new(
            request = Request$new(z$request$method,
                                  z$request$uri,
                                  z$request$body$string,
                                  z$request$headers,
                                  disk = z$response$body$file),
            response = response
          )
          hash <- zz$to_hash()
          if (request_ignorer$should_be_ignored(hash$request)) NULL else hash
        }))
        return(tmp)
      } else {
        return(list())
      }
    },

    #' @description write recorded interactions to disk
    #' @return nothing returned
    write_recorded_interactions_to_disk = function() {
      if (!self$any_new_recorded_interactions()) return(NULL)
      hash <- self$serializable_hash()
      if (length(hash[["http_interactions"]]) == 0) return(NULL)
      fun <- self$serializer$serialize()
      fun(hash[[1]], self$persister$file_name, self$preserve_exact_body_bytes)
    },

    #' @description record an http interaction (doesn't write to disk)
    #' @param x an crul or httr response object, with the request at `$request`
    #' @return nothing returned
    record_http_interaction = function(x) {
      int <- self$make_http_interaction(x)
      self$http_interactions_$response_for(int$request)
      vcr_log_info(sprintf("   Recorded HTTP interaction: %s => %s",
        request_summary(int$request), response_summary(int$response)),
        vcr_c$log_opts$date)
      self$new_recorded_interactions <- c(self$new_recorded_interactions, int)
    },

    #' @description Are there any new recorded interactions?
    #' @return logical
    any_new_recorded_interactions = function() {
      length(self$new_recorded_interactions) != 0
    },

    #' @description make list of all options
    #' @return nothing returned
    make_args = function() {
      self$args <- list(
        record = self$record,
        match_requests_on = self$match_requests_on,
        re_record_interval = self$re_record_interval,
        tag = self$tag, tags = self$tags,
        update_content_length_header = self$update_content_length_header,
        decode_compressed_response = self$decode_compressed_response,
        allow_playback_repeats = self$allow_playback_repeats,
        allow_unused_http_interactions = self$allow_unused_http_interactions,
        exclusive = self$exclusive, serialize_with = self$serialize_with,
        persist_with = self$persist_with,
        preserve_exact_body_bytes = self$preserve_exact_body_bytes
      )
    },

    #' @description write metadata to the cassette
    #' @return nothing returned
    write_metadata = function() {
      aa <- c(name = self$name, self$args)
      for (i in seq_along(aa)) {
        cat(sprintf("%s: %s", names(aa[i]), aa[i]),
            file = sprintf("%s/%s_metadata.yml",
                           path.expand(cassette_path()), self$name),
            sep = "\n", append = TRUE)
      }
    },

    #' @description make [HTTPInteractionList] object, assign to http_interactions_ var
    #' @return nothing returned
    http_interactions = function() {
      self$http_interactions_ <- HTTPInteractionList$new(
        interactions = {
          if (self$should_stub_requests()) {
            self$previously_recorded_interactions()
          } else {
            list()
          }
        },
        request_matchers = self$match_requests_on
        # request_matchers = vcr_configuration()$match_requests_on
      )
    },

    #' @description Make an `HTTPInteraction` object
    #' @param x an crul or httr response object, with the request at `$request`
    #' @return an object of class [HTTPInteraction]
    make_http_interaction = function(x) {
      # content must be raw or character
      assert(unclass(x$content), c('raw', 'character'))
      new_file_path <- ""
      is_disk <- FALSE
      if (is.character(x$content)) {
        if (file.exists(x$content)) {
          is_disk <- TRUE
          write_disk_path <- vcr_c$write_disk_path
          if (is.null(write_disk_path))
            stop("if writing to disk, write_disk_path must be given; ",
              "see ?vcr_configure")
          write_disk_path <- normalizePath(write_disk_path, mustWork=TRUE)
          new_file_path <- file.path(write_disk_path, basename(x$content))
        }
      }

      request <- Request$new(
        method = x$request$method,
        uri = x$url,
        body = if (inherits(x, "response")) { # httr
          bd <- webmockr::pluck_body(x$request)
          if (inherits(bd, "raw")) rawToChar(bd) else bd
        } else { # crul
          webmockr::pluck_body(x$request)
        },
        headers = if (inherits(x, "response")) {
          as.list(x$request$headers)
        } else {
          x$request_headers
        },
        opts = self$cassette_opts,
        disk = is_disk
      )

      response <- VcrResponse$new(
        status = if (inherits(x, "response")) {
          c(list(status_code = x$status_code), httr::http_status(x))
        } else unclass(x$status_http()),
        headers = if (inherits(x, "response")) x$headers else x$response_headers,
        body = if (is.raw(x$content)) {
          if (can_rawToChar(x$content)) rawToChar(x$content) else x$content
        } else {
          stopifnot(inherits(unclass(x$content), "character"))
          if (file.exists(x$content)) {
            # calculate new file path in fixtures/
            # copy file into fixtures/file_cache/
            # don't move b/c don't want to screw up first use before using
            # cached request
            file.copy(x$content, write_disk_path,
              overwrite = TRUE, recursive = TRUE) # copy the file
            new_file_path
            # raw(0)
          } else {
            x$content
          }
        },
        http_version = if (inherits(x, "response")) {
          x$all_headers[[1]]$version
        } else {
          x$response_headers$status
        },
        opts = self$cassette_opts,
        disk = is_disk
      )
      if (self$update_content_length_header)
        response$update_content_length_header()
      HTTPInteraction$new(request = request, response = response)
    },

    #' @description Make a crul response object
    #' @return a crul response
    serialize_to_crul = function() {
      if (length(self$deserialized_hash()) != 0) {
        intr <- self$deserialized_hash()[[1]][[1]]
      } else {
        intr <- tryCatch(
          self$previously_recorded_interactions()[[1]],
          error = function(e) e
        )
        if (inherits(intr, "error")) {
          intr <- tryCatch(
            self$new_recorded_interactions[[1]],
            error = function(e) e
          )
          if (inherits(intr, "error")) {
            stop("no requests found to construct a crul response")
          }
        }
      }

      # request
      req <- webmockr::RequestSignature$new(
        method = intr$request$method,
        uri = intr$request$uri,
        options = list(
          body = intr$request$body %||% NULL,
          headers = intr$request$headers %||% NULL,
          proxies = NULL,
          auth = NULL
        )
      )

      # response
      resp <- webmockr::Response$new()
      resp$set_url(intr$request$uri)
      bod <- intr$response$body
      resp$set_body(if ("string" %in% names(bod)) bod$string else bod)
      resp$set_request_headers(intr$request$headers)
      resp$set_response_headers(intr$response$headers)
      resp$set_status(status = intr$response$status$status_code %||% 200)

      # generate crul response
      webmockr::build_crul_response(req, resp)
    }

  )
)
