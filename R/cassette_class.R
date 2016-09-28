#' Cassette handler
#'
#' @export
#' @param name (character) A cassette name
#' @param record (character) Record mode. See \link{recording}
#' @param manfile (character) path to man file for the cassette
#' @param recorded_at (character) time recorded at
#' @param serialize_with (character) Only choice is "yaml"
#' @param persist_with Only choice is FileSystem
#' @param root_dir Root directory for storing cassettes. Default: \code{~/vcr/vcr_cassettes}
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
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' vcr_configure()
#'
#' res <- Cassette$new(name = "bob")
#' res <- Cassette$new("foobar", record = "all")
#' res$file()
#' res$originally_recorded_at()
#' res$recording()
#' res$serializable_hash()
#' res$eject()
#' res$should_remove_matching_existing_interactions()
#' res$storage_key()
#' }
Cassette <- R6::R6Class("Cassette",
  public = list(
    name = NA,
    record = "all",
    manfile = NA,
    recorded_at = NA,
    serialize_with = "yaml",
    serializer = NA,
    persist_with = "FileSystem",
    persister = NA,
    root_dir = "~/vcr/vcr_cassettes",
    match_requests_on = NA,
    re_record_interval = NA,
    tag = NA,
    tags = NA,
    update_content_length_header = FALSE,
    decode_compressed_response = FALSE,
    allow_playback_repeats = FALSE,
    allow_unused_http_interactions = TRUE,
    exclusive = FALSE,
    preserve_exact_body_bytes = TRUE,
    args = list(),
    http_interactions_ = NULL,
    new_recorded_interactions = NULL,
    new_recording = FALSE,
    to_return = NULL,

    initialize = function(name, record, serialize_with = "yaml", persist_with = "FileSystem",
        root_dir = "~/vcr/vcr_cassettes", match_requests_on = c("method", "uri"),
        re_record_interval, tag, tags, update_content_length_header,
        decode_compressed_response, allow_playback_repeats, allow_unused_http_interactions,
        exclusive, preserve_exact_body_bytes, new_recording = FALSE) {

      self$name <- name
      self$new_recording <- FALSE
      self$root_dir <- root_dir
      self$serialize_with <- serialize_with
      self$persist_with <- persist_with
      if (!missing(record)) self$record <- record
      self$make_dir()
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()), self$name)
      if (!file.exists(self$manfile)) cat("\n", file = self$manfile)
      if (!missing(match_requests_on)) self$match_requests_on = match_requests_on
      if (!missing(re_record_interval)) self$re_record_interval = re_record_interval
      if (!missing(tag)) self$tag = tag
      if (!missing(tags)) self$tags = tags
      if (!missing(update_content_length_header)) self$update_content_length_header = update_content_length_header
      if (!missing(decode_compressed_response)) self$decode_compressed_response = decode_compressed_response
      if (!missing(allow_playback_repeats)) self$allow_playback_repeats = allow_playback_repeats
      if (!missing(allow_unused_http_interactions)) self$allow_unused_http_interactions = allow_unused_http_interactions
      if (!missing(exclusive)) self$exclusive = exclusive
      if (!missing(preserve_exact_body_bytes)) self$preserve_exact_body_bytes = preserve_exact_body_bytes
      self$make_args()
      if (!file.exists(self$manfile)) self$write_metadata()
      self$recorded_at <- file.info(self$file())$mtime
      self$serializer = serializer_fetch(self$serialize_with, self$name)
      self$persister = persister_fetch(self$persist_with, self$serializer$path)
      message("Initialized with options: ", self$record)

      # create new env for recorded interactions
      self$new_recorded_interactions <- list()
    },

    print = function() {
      cat(paste0("<vcr - Cassette> ", self$name), sep = "\n")
      cat(paste0("  Record method: ", self$record), sep = "\n")
      cat(paste0("  Serialize with: ", self$serialize_with), sep = "\n")
      cat(paste0("  Persist with: ", self$persist_with), sep = "\n")
      cat(paste0("  update_content_length_header: ", self$update_content_length_header), sep = "\n")
      cat(paste0("  decode_compressed_response: ", self$decode_compressed_response), sep = "\n")
      cat(paste0("  allow_playback_repeats: ", self$allow_playback_repeats), sep = "\n")
      cat(paste0("  allow_unused_http_interactions: ", self$allow_unused_http_interactions), sep = "\n")
      cat(paste0("  exclusive: ", self$exclusive), sep = "\n")
      cat(paste0("  preserve_exact_body_bytes: ", self$preserve_exact_body_bytes), sep = "\n")
      invisible(self)
    },

    call_block = function(...) {
      # capture block
      # tmp <- lazyeval::all_dots(lazyeval::lazy_dots({
      #   httr::GET("http://google.com")
      # }))
      # tmp <- lazyeval::all_dots(lazyeval::lazy_dots(httr::GET("http://google.com")))
      # tmp <- lazyeval::lazy_dots(httr::GET("http://api.crossref.org/works"))
      # tmp2 <- lazyeval::lazy_dots(httr::GET("http://google.com", query = list(a = 5)))
      # tmp <- lazyeval::all_dots(lazyeval::lazy_dots(...))
      tmp <- lazyeval::lazy_dots(...)
      # try to match - if no match found, proceed with evaluating call
      ## old interactions
      old <- self$previously_recorded_interactions()
      # cat(paste0("Old: ", length(old)), sep = "\n")
      ## new interactions data collection
      #new <- self$parse_http(tmp[[1]]$expr)

      #return(tmp[[1]]$expr)
      # cat(paste0("tmp values: ", paste0(as.character(tmp[[1]]$expr), collapse = " -- ")), sep = "\n")
      # cat(paste0("tmp class: ", class(tmp[[1]]$expr)), sep = "\n")
      new <- self$parse_http(self$evallazy(tmp[[1]]$expr))
      # cat(paste0("New: ", length(new)), sep = "\n")
      # cat(paste0("New: ", paste0(names(new), collapse = ",")), sep = "\n")
      # cat(paste0("New method: ", new$method), sep = "\n")
      # cat(paste0("New uri: ", new$uri), sep = "\n")
      # cat(paste0("New query: ", new$query), sep = "\n")
      ## matchers
      matchby <- vcr_c$match_requests_on
      # cat(paste0("matchby: ", paste0(matchby,collapse = ",")), sep = "\n")
      # cat(paste0("matchers: ", paste0(names(request_matchers$registry), collapse = ",")), sep = "\n")

      # do checks
      iold <- list()
      res <- c()
      for (i in seq_along(old)) {
        # cat(paste0("old names: ", names(old[[i]])), sep = "\n")
        # cat(new$method, sep = "\n")
        # cat(old[[i]]$request$method, sep = "\n")
        for (j in seq_along(matchby)) {
          res[j] <- request_matchers$registry[[matchby[j]]]$matches(new, old[[i]]$request)
        }
        # cat(paste0(res, collapse = ","))

        iold[i] <- if (any(unlist(res))) {
          old[i]
        } else {
          ""
        }
      }
      iold <- Filter(function(x) inherits(x, "list"), iold)
      # cat(paste0("iold: ", length(iold)), sep = "\n")

      if (length(iold) == 0) {
        # not using a cached response
        self$new_recording <- TRUE

        # evaluate block if no matches
        out <- eval(tmp[[1]]$expr)
        # record interaction
        self$record_http_interaction(out)
        #return(out)
        self$to_return <- out
      } else {
        # if matches return recorded results
        ## FIXME - when there's a match deal with what interactions to return
        #return(self$build_httr_response(iold[[1]]))
        self$to_return <- self$build_httr_response(iold[[1]])
      }

      self$eject()
    },

    evallazy = function(x) {
      if (x[[1]] == "{") {
        x[[2]]
      } else {
        x
      }
    },

    parse_http = function(x) {
      tmp <- as.list(x)
      uri <- get_uri(tmp)
      list(
        method = get_method(tmp),
        uri = uri,
        host = get_host(uri),
        path = get_path(uri),
        query = get_query(tmp),
        body = get_body(tmp)
      )
    },

    build_httr_response = function(x) {
      # fixme, not reading yaml back in correctly
      httr:::response(
        url = x$request$uri,
        status_code = as.numeric(strex(x$response$status$message, "[0-9]{3}")),
        headers = x$response$headers,
        all_headers = x$response$headers,
        cookies = '',
        # FIXME - be able to toggle whether to base64 decode here
        content = self$conv_body(x$response$body),
        #content = base64enc::base64decode(x$response$body),
        date = strptime(x$response$recorded_at, "%Y-%m-%d %H:%M:%S", tz = "GMT"),
        times = NULL,
        request = httr:::request_build(x$request$method, x$request$uri),
        handle = httr::handle(x$request$uri)
      )
    },

    conv_body = function(x) {
      switch(
        class(x),
        raw = x,
        character = charToRaw(x)
      )
    },

    eject = function() {
      # write interactions to disk
      ###### FIXME - don't write records to disk if using recorded records ####
      if (self$new_recording) {
        self$write_recorded_interactions_to_disk()
      }
      # remove cassette from list of current cassettes
      rm(list = self$name, envir = vcr_cassettes)
      return(self$to_return)
    },

    file = function() {
      self$manfile
    },

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

      # We deep-dup the interactions by roundtripping them to/from a hash.
      # This is necessary because `before_record` can mutate the interactions.
      # lapply(self$merged_interactions(), function(z) {
      #   VCRHooks$invoke_hook("before_record", z)
      # })
    },

    merged_interactions = function() {
      old_interactions <- self$previously_recorded_interactions()

      if (self$should_remove_matching_existing_interactions()) {
        new_interaction_list <-
          HTTPInteractionList$new(self$new_recorded_interactions, self$match_requests_on)
        old_interactions <-
          Filter(function(x) new_interaction_list$response_for(x$request), old_interactions)
      }

      return(c(old_interactions, self$new_recorded_interactions))
    },

    should_remove_matching_existing_interactions = function() {
      self$record == "all"
    },

    storage_key = function() {
      self$serializer$path
    },

    make_dir = function() {
      dir.create(path.expand(self$root_dir), showWarnings = FALSE, recursive = TRUE)
    },

    deserialized_hash = function() {
      tmp <- self$serializer$deserialize_path()
      if (inherits(tmp, "list")) {
        return(tmp)
      } else {
        stop(tmp, " does not appear to be a valid cassette", call. = FALSE)
      }
    },

    # serializer = serializer_fetch(serialize_with, name)
    # tmp = serializer$deserialize_path()
    # z = tmp[['http_interactions']][[1]]
    previously_recorded_interactions = function() {
      #if (!raw_cassette_bytes.to_s.empty?) {
      lapply(self$deserialized_hash()[['http_interactions']], function(z) {
        zz <- HTTPInteraction$new(
          request = Request$new(z$request$method, z$request$uri, z$request$body, z$request$headers),
          response = Response$new(z$response$status, z$response$headers, z$response$body$string)
        )
        zz$to_hash()
        # FIXME - not quite ready yet, request_ignorer not quite working
        # if ( tmpints$request$uri && request_ignorer$ignore(tmpints$request) ) {
        #
        # }
      })
    },

    write_recorded_interactions_to_disk = function(x) {
      if (!self$any_new_recorded_interactions()) return(NULL)
      hash <- self$serializable_hash()
      if (length(hash[["http_interactions"]]) == 0) return(NULL)
      self$persister <-
        FileSystem$new(
          file_name = self$serializer$path,
          write_fxn = self$serializer$serialize(),
          content = hash$http_interactions,
          path = self$serializer$path,
          write2disk = TRUE
        )
      # FileSystem$new(
      #   file_name = cassette$serializer$path,
      #   write_fxn = cassette$serializer$serialize(),
      #   content = hash$http_interactions,
      #   path = cassette$serializer$path
      # )
    },

    record_http_interaction = function(x) {
      #do.call(loggr::log_file, list(file_name = vcr_c$vcr_logging, vcr_c$vcr_logging_opts))
      int <- self$make_http_interaction(x)
      loggr::log_info(self$log_prepare(x))
      loggr::log_info(sprintf("   Recorded HTTP interaction: %s => %s",
                              int$request$uri, response_summary(x)))
      self$new_recorded_interactions <- c(self$new_recorded_interactions, int)
    },

    any_new_recorded_interactions = function() {
      length(self$new_recorded_interactions) != 0
    },

    log_prepare = function(x) {
      sprintf("%s => %s", x$request$uri, x$response$status$message)
    },

    log_write = function(x) {
      message("\n", x, file = vcr_configuration()$vcr_log_path, append = TRUE)
    },

    make_args = function() {
      self$args <- list(record = self$record, match_requests_on = self$match_requests_on,
        re_record_interval = self$re_record_interval, tag = self$tag, tags = self$tags,
        update_content_length_header = self$update_content_length_header,
        decode_compressed_response = self$decode_compressed_response,
        allow_playback_repeats = self$allow_playback_repeats,
        allow_unused_http_interactions = self$allow_unused_http_interactions,
        exclusive = self$exclusive, serialize_with = self$serialize_with,
        persist_with = self$persist_with,
        preserve_exact_body_bytes = self$preserve_exact_body_bytes)
    },

    write_metadata = function() {
      aa <- c(name = self$name, self$args)
      for (i in seq_along(aa)) {
        cat(sprintf("%s: %s", names(aa[i]), aa[i]),
            file = sprintf("%s/%s_metadata.yml", path.expand(cassette_path()), self$name),
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
        x$headers)
      response <- Response$new(
        http_status(x),
        x$headers,
        httr::content(x, "text"),
        x$all_headers[[1]]$version)
      HTTPInteraction$new(request = request, response = response)
    }

  )
)
