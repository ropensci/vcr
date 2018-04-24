#' Use a cassette
#'
#' @export
#' @param name The name of the cassette. vcr will sanitize this to ensure it
#' is a valid file name.
#' @param ... a block of code to evalulate, wrapped in curly braces
#' @param record The record mode. Default: "once". In the future we'll support
#' "once", "all", "none", "new_episodes". See [recording] for more information
#' @param match_requests_on List of request matchers
#' to use to determine what recorded HTTP interaction to replay. Defaults to
#' `["method", "uri"]`. The built-in matchers are "method", "uri", "host",
#' "path", "headers" and "body"
#' @param re_record_interval (integer) When given, the
#' cassette will be re-recorded at the given interval, in seconds.
#' IGNORED FOR NOW.
#' @param tag (character) Used to apply tagged `before_record`
#' and `before_playback` hooks to the cassette. IGNORED FOR NOW.
#' @param tags Used to apply multiple tags to
#' a cassette so that tagged `before_record` and `before_playback` hooks
#' will apply to the cassette. IGNORED FOR NOW.
#' @param update_content_length_header (logical) Whether or
#' not to overwrite the `Content-Length` header of the responses to
#' match the length of the response body. Default: `FALSE`
#' @param decode_compressed_response (logical) Whether or
#' not to decode compressed responses before recording the cassette.
#' This makes the cassette more human readable. Default: `FALSE`.
#' IGNORED FOR NOW.
#' @param allow_playback_repeats (logical) Whether or not to
#' allow a single HTTP interaction to be played back multiple times.
#' Default: `FALSE`.
#' @param allow_unused_http_interactions (logical) If set to
#' false, an error will be raised if a cassette is ejected before all
#' previously recorded HTTP interactions have been used.
#' Default: `TRUE`. Note that when an error has already occurred
#' (as indicated by the `$!` variable) unused interactions will be
#' allowed so that we don't silence the original error (which is almost
#' certainly more interesting/important). IGNORED FOR NOW.
#' @param exclusive (logical) Whether or not to use only this
#'  cassette and to completely ignore any cassettes in the cassettes stack.
#'  Default: `FALSE`. IGNORED FOR NOW.
#' @param serialize_with (character) Which serializer to use.
#'  Valid values are "yaml" (default), the only one supported for now.
#' @param persist_with (character) Which cassette persister to
#'  use. Default: "file_system". You can also register and use a
#'  custom persister.
#' @param preserve_exact_body_bytes (logical) Whether or not
#' to base64 encode the bytes of the requests and responses for
#' this cassette when serializing it. See also `preserve_exact_body_bytes`
#' in [vcr_configure()]. Default: `FALSE`
#'
#' @details A run down of the family of top level \pkg{vcr} functions
#'
#' - `use_cassette` Initializes a cassett. Returns the inserted
#'  cassette.
#' - `insert_cassette` Internally used within `use_cassette`
#' - `eject_cassette` ejects the current cassette. The cassette
#'  will no longer be used. In addition, any newly recorded HTTP interactions
#'  will be written to disk.
#'
#' @seealso [insert_cassette()], [eject_cassette()]
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = "~/fixtures/vcr_cassettes")
#'
#' use_cassette(name = "apple7", {
#'   cli <- HttpClient$new(url = "https://httpbin.org")
#'   resp <- cli$get("get")
#' })
#'
#' use_cassette(name = "stuff2", {
#'   cli$post("post")
#' })
#'
#' library(rcrossref)
#' vcr_configure(dir = "~/fixtures/vcr_cassettes")
#' vcr_configuration()
#' use_cassette("crossref2", {
#'   res <- cr_works(limit = 10)
#' })
#'
#'
#' # preserve exact body bytes - records in base64 encoding
#' vcr_configure(
#'   dir = "~/fixtures/vcr_cassettes",
#'   preserve_exact_body_bytes = TRUE
#' )
#' # x <- insert_cassette("things4")
#' use_cassette("things4", {
#'   cli <- crul::HttpClient$new(url = "https://httpbin.org")
#'   bbb <- cli$get("get")
#' })
#' }

use_cassette <- function(name, ..., record = "once",
  match_requests_on = c("method", "uri"),
  re_record_interval = NULL, tag = NULL, tags = NULL,
  update_content_length_header = FALSE, decode_compressed_response = FALSE,
  allow_playback_repeats = FALSE, allow_unused_http_interactions = TRUE,
  exclusive = FALSE, serialize_with = "yaml", persist_with = "FileSystem",
  preserve_exact_body_bytes = FALSE) {

  cassette <- insert_cassette(
    name, record = record, match_requests_on = match_requests_on,
    re_record_interval = re_record_interval,
    tag = tag, tags = tags,
    update_content_length_header = update_content_length_header,
    decode_compressed_response = decode_compressed_response,
    allow_playback_repeats = allow_playback_repeats,
    allow_unused_http_interactions = allow_unused_http_interactions,
    exclusive = exclusive,
    serialize_with = serialize_with, persist_with = persist_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes)
  on.exit(cassette$eject())
  cassette$call_block(...)
}
