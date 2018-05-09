#' Cassette handler
#'
#' @export
#' @keywords internal
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
#' `["method", "uri"]`. The built-in matchers are "method", "uri", "host",
#' "path", "headers" and "body"
#' @param update_content_length_header (logical) Whether or
#' not to overwrite the `Content-Length` header of the responses to
#' match the length of the response body. Default: `FALSE`
#' @param allow_playback_repeats (logical) Whether or not to
#' allow a single HTTP interaction to be played back multiple times.
#' Default: `FALSE`.
#' @param preserve_exact_body_bytes (logical) Whether or not
#' to base64 encode the bytes of the requests and responses for
#' this cassette when serializing it. See also `preserve_exact_body_bytes`
#' in [vcr_configure()]. Default: `FALSE`
#'
#' @return an object of class `Cassette`
#'
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{eject()}}{
#'       Coming soon.
#'     }
#'     \item{\code{file()}}{
#'       Get path to the man file for the cassette.
#'     }
#'     \item{\code{recording()}}{
#'       Find out whether recording is happening or not.
#'     }
#'     \item{\code{originally_recorded_at()}}{
#'       Time interaction was originally recorded at.
#'     }
#'     \item{\code{serializable_hash()}}{
#'       A hash with stuff.
#'     }
#'     \item{\code{should_remove_matching_existing_interactions()}}{
#'       Set record mode to "all".
#'     }
#'     \item{\code{storage_key()}}{
#'       Generate file name = cassette name plus file extension.
#'     }
#'     \item{\code{make_dir()}}{
#'       Make cassette directory.
#'     }
#'     \item{\code{raw_string()}}{
#'       Get raw string of the cassette (cached interaction).
#'     }
#'     \item{\code{deserialized_hash()}}{
#'       Get a deserialized hash.
#'     }
#'     \item{\code{make_args()}}{
#'       Initialize default args.
#'     }
#'     \item{\code{write_metadata()}}{
#'       Write metadata to disk.
#'     }
#'     \item{\code{previously_recorded_interactions()}}{
#'       Coming soon.
#'     }
#'     \item{\code{serialize_to_crul}}{
#'       Serialize interaction on disk/casette to a \code{crul} response
#'     }
#'   }
#' @format NULL
#' @usage NULL
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
    name = NA,
    record = "all",
    manfile = NA,
    recorded_at = NA,
    serialize_with = "yaml",
    serializer = NA,
    persist_with = "FileSystem",
    persister = NA,
    match_requests_on = c("method", "uri"),
    re_record_interval = NULL,
    tag = NA,
    tags = NA,
    root_dir = NA,
    update_content_length_header = FALSE,
    decode_compressed_response = FALSE,
    allow_playback_repeats = FALSE,
    allow_unused_http_interactions = TRUE,
    exclusive = FALSE,
    preserve_exact_body_bytes = FALSE,
    args = list(),
    http_interactions_ = NULL,
    new_recorded_interactions = NULL,
    clean_outdated_http_interactions = FALSE,
    to_return = NULL,
    cassette_opts = NULL,

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
        recmodes <- c('none', 'once', 'new_episodes', 'all')
        if (!record %in% recmodes) {
          stop("'record' value of '", record, "' is not in the allowed set: ",
               paste0(recmodes, collapse = ", "), call. = FALSE)
        }
        self$record <- record
      }
      self$make_dir()
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()),
                              self$name)
      if (!file.exists(self$manfile)) cat("\n", file = self$manfile)
      if (!missing(match_requests_on)) {
        mro <- c("method", "uri", "headers", "host", "path", "body")
        if (!any(match_requests_on %in% mro)) {
          stop("1 or more 'match_requests_on' values (",
               paste0(match_requests_on, collapse = ", "),
               ") is not in the allowed set: ",
               paste0(mro, collapse = ", "), call. = FALSE)
        }
        # we don't yet support the following matchers: host, path, body
        if (any(match_requests_on %in% c("host", "path", "body"))) {
          stop("we do not yet support host, path, or body matchers",
            "\n see https://github.com/ropensci/vcr/issues/70",
            call. = FALSE)
        }
        self$match_requests_on <- match_requests_on
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
          if (all(m %in% c("method", "uri")) && length(m) == 2) {
            webmockr::stub_request(req$method, req$uri)
          } else if (all(m %in% c("method", "uri", "query")) && length(m) == 3) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter))
          } else if (all(m %in% c("method", "uri", "headers")) && length(m) == 3) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = req$headers))
          } else if (all(m %in% c("method", "uri", "headers", "query")) && length(m) == 4) {
            tmp <- webmockr::stub_request(req$method, req$uri)
            webmockr::wi_th(tmp, .list = list(query = uripp$parameter, headers = req$headers))
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
      init_opts <- compact(stats::setNames(tmp, c("name", "record", "serialize_with",
        "persist_with", "match_requests_on", "update_content_length_header",
        "allow_playback_repeats", "preserve_exact_body_bytes")))
      self$cassette_opts <- init_opts
      init_opts <- paste(names(init_opts), unname(init_opts), sep=": ", collapse=", ")
      vcr_log_info(sprintf("Initialized with options: {%s}", init_opts), vcr_c$log_opts$date)

      # create new env for recorded interactions
      self$new_recorded_interactions <- list()

      # put cassette in vcr_cassettes environment
      include_cassette(self)
    },

    print = function() {
      cat(paste0("<vcr - Cassette> ", self$name), sep = "\n")
      cat(paste0("  Record method: ", self$record), sep = "\n")
      cat(paste0("  Serialize with: ", self$serialize_with), sep = "\n")
      cat(paste0("  Persist with: ", self$persist_with), sep = "\n")
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

    file = function() self$manfile,

    recording = function() {
      if (self$record == "none") {
        return(FALSE)
      } else if (self$record == "once") {
        return(self$is_empty())
      } else {
        return(TRUE)
      }
    },

    is_empty = function() {
      # self$persister$is_empty()
      nchar(self$raw_cassette_bytes()) < 1
    },

    originally_recorded_at = function() {
      self$recorded_at
    },

    serializable_hash = function() {
      list(
        http_interactions = self$interactions_to_record(),
        recorded_with = utils::packageVersion("vcr")
      )
    },

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
            new_interaction_list$response_for(req)
          }, old_interactions)
      }

      # FIXME: add up_to_date_interactions usage here
      return(c(old_interactions, self$new_recorded_interactions))
    },

    # FIXME: not used yet, from newer version of vcr
    up_to_date_interactions = function(interactions) {
      if (self$clean_outdated_http_interactions &&
        !is.null(re_record_interval)
      ) {
        return(interactions)
      }
      Filter(function(z) {
        as.POSIXct(z$recorded_at) > (Sys.time() - vcr_c$re_record_interval)
      }, interactions)
    },

    should_remove_matching_existing_interactions = function() self$record == "all",
    storage_key = function() self$serializer$path,

    raw_cassette_bytes = function() {
      file <- self$file()
      if (is.null(file)) return("")
      tmp <- readLines(file) %||% ""
      paste0(tmp, collapse = "")
    },

    make_dir = function() {
      dir.create(path.expand(self$root_dir), showWarnings = FALSE,
                 recursive = TRUE)
    },

    deserialized_hash = function() {
      tmp <- self$serializer$deserialize_path()
      if (inherits(tmp, "list")) {
        return(tmp)
      } else {
        stop(tmp, " does not appear to be a valid cassette", call. = FALSE)
      }
    },

    previously_recorded_interactions = function() {
      if (nchar(self$raw_cassette_bytes()) > 0) {
        tmp <- compact(lapply(self$deserialized_hash()[['http_interactions']], function(z) {
          response <- VcrResponse$new(
            z$response$status$status_code,
            z$response$headers,
            z$response$body$string,
            self$cassette_opts
          )
          if (self$update_content_length_header) response$update_content_length_header()
          zz <- HTTPInteraction$new(
            request = Request$new(z$request$method,
                                  z$request$uri,
                                  z$request$body$string,
                                  z$request$headers),
            response = response
          )
          hash <- zz$to_hash()
          # FIXME: not quite ready yet, request_ignorer not quite working
          if (request_ignorer$should_be_ignored(hash$request)) NULL else hash
        }))
        return(tmp)
      } else {
        return(list())
      }
    },

    write_recorded_interactions_to_disk = function() {
      if (!self$any_new_recorded_interactions()) return(NULL)
      hash <- self$serializable_hash()
      if (length(hash[["http_interactions"]]) == 0) return(NULL)
      fun <- self$serializer$serialize()
      fun(hash[[1]], self$persister$file_name, self$preserve_exact_body_bytes)
    },

    record_http_interaction = function(x) {
      int <- self$make_http_interaction(x)
      self$http_interactions_$response_for(int$request)
      vcr_log_info(sprintf("   Recorded HTTP interaction: %s => %s",
        request_summary(int$request), response_summary(int$response)),
        vcr_c$log_opts$date)
      self$new_recorded_interactions <- c(self$new_recorded_interactions, int)
    },

    any_new_recorded_interactions = function() {
      length(self$new_recorded_interactions) != 0
    },

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

    write_metadata = function() {
      aa <- c(name = self$name, self$args)
      for (i in seq_along(aa)) {
        cat(sprintf("%s: %s", names(aa[i]), aa[i]),
            file = sprintf("%s/%s_metadata.yml",
                           path.expand(cassette_path()), self$name),
            sep = "\n", append = TRUE)
      }
    },

    http_interactions = function() {
      self$http_interactions_ <- HTTPInteractionList$new(
        interactions = self$previously_recorded_interactions(),
        request_matchers = vcr_configuration()$match_requests_on
      )
    },

    make_http_interaction = function(x) {
      request <- Request$new(
        x$request$method,
        x$url,
        x$body,
        x$request_headers,
        self$cassette_opts
      )
      response <- VcrResponse$new(
        x$status_http(),
        headers = x$response_headers,
        body = rawToChar(x$content),
        http_version = x$response_headers$status,
        self$cassette_opts
      )
      if (self$update_content_length_header) response$update_content_length_header()
      HTTPInteraction$new(request = request, response = response)
    },

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
