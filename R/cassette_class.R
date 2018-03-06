#' Cassette handler
#'
#' @export
#' @param name (character) A cassette name
#' @param record (character) Record mode. See \link{recording}
#' @param manfile (character) path to man file for the cassette
#' @param recorded_at (character) time recorded at
#' @param serialize_with (character) Only choice is "yaml"
#' @param persist_with Only choice is FileSystem
#' @param match_requests_on what to match requests on
#' @param re_record_interval Interval to re-record
#' @param tag Tag
#' @param tags Tags
#' @param update_content_length_header xx Default: \code{FALSE}
#' @param decode_compressed_response xx Default: \code{FALSE}
#' @param allow_playback_repeats xx Default: \code{FALSE}
#' @param allow_unused_http_interactions xx Default: \code{TRUE}
#' @param exclusive xx Default: \code{FALSE}
#' @param preserve_exact_body_bytes xx Default: \code{TRUE}
#' @param args A list of args
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
#' @details
#' The root directory for storing cassettes. Default:
#' `~/fixtures/vcr_cassettes`. See [vcr_configure()]
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = "~/fixtures/vcr_cassettes")
#'
#' res <- Cassette$new(name = "bob")
#' res$file()
#' res$originally_recorded_at()
#' res$recording()
#' res$serializable_hash()
#' res$eject()
#' res$should_remove_matching_existing_interactions()
#' res$storage_key()
#'
#' new <- res$call_block({
#'   cli <- HttpClient$new(url = "https://httpbin.org")
#'   resp <- cli$get("get")
#' })
#'
#' # record all requests
#' res <- Cassette$new("foobar", record = "all")
#' }
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
    match_requests_on = NA,
    re_record_interval = NA,
    tag = NA,
    tags = NA,
    root_dir = NA,
    update_content_length_header = FALSE,
    decode_compressed_response = FALSE,
    allow_playback_repeats = FALSE,
    allow_unused_http_interactions = TRUE,
    exclusive = FALSE,
    preserve_exact_body_bytes = TRUE,
    args = list(),
    http_interactions_ = NULL,
    new_recorded_interactions = NULL,
    to_return = NULL,

    initialize = function(
      name, record, serialize_with = "yaml",
      persist_with = "FileSystem",
      match_requests_on = c("method", "uri"),
      re_record_interval, tag, tags, update_content_length_header,
      decode_compressed_response, allow_playback_repeats,
      allow_unused_http_interactions, exclusive, preserve_exact_body_bytes) {

      self$name <- name
      self$root_dir <- vcr_configuration()$dir
      self$serialize_with <- serialize_with
      self$persist_with <- persist_with
      if (!missing(record)) self$record <- record
      self$make_dir()
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()),
                              self$name)
      if (!file.exists(self$manfile)) cat("\n", file = self$manfile)
      if (!missing(match_requests_on))
        self$match_requests_on <- match_requests_on
      if (!missing(re_record_interval))
        self$re_record_interval <- re_record_interval
      if (!missing(tag)) self$tag = tag
      if (!missing(tags)) self$tags = tags
      if (!missing(update_content_length_header))
        self$update_content_length_header = update_content_length_header
      if (!missing(decode_compressed_response))
        self$decode_compressed_response = decode_compressed_response
      if (!missing(allow_playback_repeats))
        self$allow_playback_repeats = allow_playback_repeats
      if (!missing(allow_unused_http_interactions))
        self$allow_unused_http_interactions = allow_unused_http_interactions
      if (!missing(exclusive)) self$exclusive = exclusive
      if (!missing(preserve_exact_body_bytes))
        self$preserve_exact_body_bytes = preserve_exact_body_bytes
      self$make_args()
      if (!file.exists(self$manfile)) self$write_metadata()
      self$recorded_at <- file.info(self$file())$mtime
      self$serializer = serializer_fetch(self$serialize_with, self$name)
      self$persister = persister_fetch(self$persist_with, self$serializer$path)

      # get previously recorded interactions
      ## if none pass, if some found, make webmockr stubs
      prev <- self$previously_recorded_interactions()
      if (length(prev) > 0) {
        invisible(lapply(prev, function(z) {
          req <- z$request
          res <- z$response
          urip <- crul::url_parse(req$uri)
          m <- vcr_c$match_requests_on
          if (all(m == c("method", "uri")) && length(m) == 2) {
            webmockr::stub_request(req$method, req$uri)
          } else if (all(m == c("method", "uri", "query")) && length(m) == 3) {
            webmockr::stub_request(req$method, req$uri) %>% webmockr::wi_th(query = urip$parameter)
          } else if (all(m == c("method", "uri", "headers")) && length(m) == 3) {
            webmockr::stub_request(req$method, req$uri) %>% webmockr::wi_th(headers = req$headers)
          } else if (all(m == c("method", "uri", "headers", "query")) && length(m) == 4) {
            webmockr::stub_request(req$method, req$uri) %>%
              webmockr::wi_th(
                query = urip$parameter,
                headers = req$headers
              )
          }
        }))
      }

      message("Initialized with options: ", self$record)

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
      # allow http interactions - disallow at end of call_block() below
      webmockr::webmockr_allow_net_connect()
      # evaluate request
      resp <- lazyeval::lazy_eval(tmp)
      # disallow http interactions - allow at start of call_block() above
      webmockr::webmockr_disable_net_connect()
    },

    eject = function() {
      # FIXME:
      #  - don't write records to disk if using recorded records
      self$write_recorded_interactions_to_disk()

      # remove cassette from list of current cassettes
      rm(list = self$name, envir = vcr_cassettes)
      message("ejecting cassette: ", self$name)
      # disable webmockr
      webmockr::disable()
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
      self$persister$is_empty()
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

      # FIXME - not sure what's going on here
      # We dee-dupe the interactions by roundtripping them to/from a hash.
      # This is necessary because `before_record` can mutate the interactions.
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
          Filter(function(x) new_interaction_list$response_for(x$request),
                 old_interactions)
      }

      # FIXME: add up_to_date_interactions usage here
      return(c(old_interactions, self$new_recorded_interactions))
    },

    # FIXME: not used yet, from newer version of vcr
    up_to_date_interactions = function(interactions) {
      if (clean_outdated_http_interactions && re_record_interval) return(interactions)
      # return interactions unless clean_outdated_http_interactions && re_record_interval
      Filter(function(z) {
        as.POSIXct(z$recorded_at) > (Sys.time() - vcr_c$re_record_interval)
      }, interactions)
      # interactions.take_while { |x| x[:recorded_at] > Time.now - re_record_interval }
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
        compact(lapply(self$deserialized_hash()[['http_interactions']], function(z) {
          zz <- HTTPInteraction$new(
            request = Request$new(z$request$method,
                                  z$request$uri,
                                  z$request$body$string,
                                  z$request$headers),
            response = VcrResponse$new(z$response$status$code,
                                       z$response$headers,
                                       z$response$body$string)
          )
          hash <- zz$to_hash()
          # FIXME - not quite ready yet, request_ignorer not quite working
          if (request_ignorer$should_be_ignored(hash$request)) NULL else hash
        }))
      } else {
        return(list())
      }
    },

    write_recorded_interactions_to_disk = function() {
      if (!self$any_new_recorded_interactions()) return(NULL)
      hash <- self$serializable_hash()
      if (length(hash[["http_interactions"]]) == 0) return(NULL)
      fun <- self$serializer$serialize()
      fun(hash[[1]], self$persister$file_name)
    },

    record_http_interaction = function(x) {
      # FIXME: fix logging at some point
      #do.call(loggr::log_file, list(file_name = vcr_c$vcr_logging, vcr_c$vcr_logging_opts))
      int <- self$make_http_interaction(x)
      #loggr::log_info(self$log_prepare(x))
      #loggr::log_info(sprintf("   Recorded HTTP interaction: %s => %s",
      #                        int$request$uri, response_summary(x)))
      self$new_recorded_interactions <- c(self$new_recorded_interactions, int)
    },

    any_new_recorded_interactions = function() {
      length(self$new_recorded_interactions) != 0
    },

    # no logging stuff used for now
    log_prepare = function(x) {
      sprintf("%s => %s", x$request$uri, x$response$status$message)
    },

    log_write = function(x) {
      message("\n", x, file = vcr_configuration()$vcr_log_path, append = TRUE)
    },

    log_prefix = function(x) {
      sprintf("[Cassette: '%s']", self$name) %||% ""
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
        interactions = list(self$previously_recorded_interactions()),
        request_matchers = vcr_configuration()$match_requests_on
      )
    },

    make_http_interaction = function(x) {
      request <- Request$new(
        x$request$method,
        x$url,
        x$body,
        x$request_headers)
      response <- VcrResponse$new(
        x$status_http(),
        headers = x$response_headers,
        body = x$parse("UTF-8"),
        http_version = x$response_headers$status)
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
