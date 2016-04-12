#' Cassette handler
#'
#' @export
#' @keywords internal
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
#' res <- Cassette$new("teddybear")
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
    record = "none",
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

    initialize = function(name, record, serialize_with = "yaml", persist_with = "FileSystem",
        root_dir = "~/vcr/vcr_cassettes", match_requests_on,
        re_record_interval, tag, tags, update_content_length_header,
        decode_compressed_response, allow_playback_repeats, allow_unused_http_interactions,
        exclusive, preserve_exact_body_bytes) {

      self$name <- name
      self$root_dir <- root_dir
      self$serialize_with <- serialize_with
      self$persist_with <- persist_with
      if (!missing(record)) self$record <- record
      self$make_dir()
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()), self$name)
      cat("\n", file = self$manfile)
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
      self$write_metadata()
      self$recorded_at <- file.info(self$file())$mtime
      self$serializer = serializer_fetch(self$serialize_with, self$name)
      self$persister = persister_fetch(self$persist_with)
      message("Initialized with options: ", self$record)
    },

    print = function() {
      cat("<vcr - Cassette> ", self$name, sep = "")
      invisible(self)
    },

    eject = function() {
      stop("coming soon...")
      write_recorded_interactions_to_disk()
    },
    file = function() {
      self$manfile
    },
    recording = function() {
      if (self$record == "none") {
        FALSE
      } else {
        TRUE
      }
    },
    originally_recorded_at = function() {
      self$recorded_at
    },
    serializable_hash = function() {
      list(
        http_interactions = "fixme",
        # interactions_to_record.map(&:to_hash),
        recorded_with = packageVersion("vcr")
      )
    },
    should_remove_matching_existing_interactions = function() {
      self$record == "all"
    },
    storage_key = function() {
      paste0(self$name, self$serializer$file_extension)
    },
    make_dir = function() {
      dir.create(path.expand(self$root_dir), showWarnings = FALSE, recursive = TRUE)
    },
    raw_string = function() {
      # in place of raw_cassette_bytes()
      readLines(self$file)
    }
    ,
    deserialized_hash = function() {
      tmp <- self$serializer$deserialize_path()
      if (inherits(tmp, "list") && length(tmp$http_interactions) > 0) {
        return(tmp)
      } else {
        stop(tmp, " does not appear to be a valid cassette", call. = FALSE)
      }
    },

    # previously_recorded_interactions = function() {
    #   #if (!raw_cassette_bytes.to_s.empty?) {
    #   lapply(self$deserialized_hash()[['http_interactions']], function(z) {
    #     zz <- HTTPInteraction$new(request = z$request, response = z$response)
    #     zz$from_hash()
    #     # lapply(HTTPInteraction$from_hash(z), function(y) {
    #     #   Filter(function(w) w$, y)
    #     # })
    #   })
    #   # deserialized_hash['http_interactions'].map { |h| HTTPInteraction.from_hash(h) } do |int|
    #     invoke_hook("before_playback", int)
    #
    #     int.reject! do |i|
    #       i.request.uri.is_a?(String) && VCRConfig$request_ignorer.ignore?(i.request)
    #     end
    #   end
    # },

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
    }
  )
)
