#' Use a cassette
#'
#' @export
#' @param name The name of the cassette. vcr will sanitize this to ensure it
#' is a valid file name.
#' @param ... a block of code to evalulate, wrapped in curly braces
#' @param record The record mode. One of "all", "none", "new_episodes", "once".
#' See [recording]
#' @param match_requests_on List of request matchers
#'  to use to determine what recorded HTTP interaction to replay. Defaults to
#'  ("method", "uri"). The built-in matchers are "method", "uri", "host",
#'  "path", "headers" and "body". You can also pass the name of a registered
#'  custom request matcher or any object that responds to #call.
#' @param re_record_interval (integer) When given, the
#'  cassette will be re-recorded at the given interval, in seconds.
#' @param tag (character) Used to apply tagged `before_record`
#'  and `before_playback` hooks to the cassette.
#' @param tags Used to apply multiple tags to
#'  a cassette so that tagged `before_record` and `before_playback` hooks
#'  will apply to the cassette.
#' @param update_content_length_header (logical) Whether or
#'  not to overwrite the Content-Length header of the responses to
#'  match the length of the response body. Defaults to false.
#' @param decode_compressed_response (logical) Whether or
#'  not to decode compressed responses before recording the cassette.
#'  This makes the cassette more human readable. Defaults to false.
#' @param allow_playback_repeats (logical) Whether or not to
#'  allow a single HTTP interaction to be played back multiple times.
#'  Defaults to false.
#' @param allow_unused_http_interactions (logical) If set to
#'  false, an error will be raised if a cassette is ejected before all
#'  previously recorded HTTP interactions have been used.
#'  Defaults to true. Note that when an error has already occurred
#'  (as indicated by the `$!` variable) unused interactions will be
#'  allowed so that we don't silence the original error (which is almost
#'  certainly more interesting/important).
#' @param exclusive (logical) Whether or not to use only this
#'  cassette and to completely ignore any cassettes in the cassettes stack.
#'  Default: FALSE.
#' @param serialize_with (character) Which serializer to use.
#'  Valid values are "yaml" (default), "syck", "psych", "json" or any registered
#'  custom serializer.
#' @param persist_with (character) Which cassette persister to
#'  use. Defaults: "file_system". You can also register and use a
#'  custom persister.
#' @param preserve_exact_body_bytes (logical) Whether or not
#'  to base64 encode the bytes of the requests and responses for this cassette
#'  when serializing it. See also `VCR::Configuration#preserve_exact_body_bytes`
#'
#' @details
#' \itemize{
#'  \item `use_cassette` Initialize a cassett. Returns the inserted
#'  cassette.
#'  \item `insert_cassette` Internally used within `use_cassette`
#'  \item `eject_cassette` ejects the current cassette. The cassette
#'  will no longer be used. In addition, any newly recorded HTTP interactions
#'  will be written to disk.
#' }
#' @seealso [insert_cassette], [eject_cassette]
#' @examples \dontrun{
#' webmockr::enable()
#' crul::mock()
#' library("crul")
#' url <- "http://api.plos.org/search?q=*:*&wt=json"
#' # url <- "http://localhost:9200"
#'
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
#' use_cassette(name = "helium", {
#'   cli$get("get", query = list(foo = "bar"))
#' })
#'
#' # sacbox::load_defaults(use_cassette)
#' # name = "skycheese"
#' # cassette$call_block(cli$get("get"))
#'
# library(crul)
# vcr_configure(dir = "~/fixtures/vcr_cassettes")
# aaa
# rm(aaa)
# rm(list = "farts2", envir = vcr_cassettes)
# library(testthat)
# use_cassette("farts2", {
#   cli <- HttpClient$new(url = "https://httpbin.org")
#   aaa <- cli$get("get")
#   #expect_is(aaa, "asdfd")
# })
#
# vcr_configure(dir = "~/fixtures/vcr_cassettes")
# use_cassette("farts9", {
#   cli <- HttpClient$new(url = "https://httpbin.org")
#   bbb <- cli$get("get")
# })
# library(rcrossref)
# vcr_configure(dir = "~/fixtures/vcr_cassettes")
# vcr_configuration()
# use_cassette("crossref2", {
#   res <- cr_works(limit = 10)
# })
#' }

use_cassette <- function(name, ..., record = "once", match_requests_on = NULL,
  re_record_interval = NULL, tag = NULL, tags = NULL,
  update_content_length_header = FALSE, decode_compressed_response = FALSE,
  allow_playback_repeats = FALSE, allow_unused_http_interactions = TRUE,
  exclusive = FALSE, serialize_with = "yaml", persist_with = "FileSystem",
  preserve_exact_body_bytes = TRUE) {

  # insert cassette - returns cassette class object
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
  # call block
  cassette$call_block(...)
  # eject cassette - records any new interactions to cassettes (i.e., disk)
  cassette$eject()
}
