#' Insert a cassette to record HTTP requests
#'
#' @export
#' @param name The name of the cassette. vcr will sanitize this to ensure it
#' is a valid file name.
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
#'  when serializing it. See also `VCR::Configuration#preserve_exact_body_bytes`.
#' @param ignore_cassettes (logical) xx. Default: `TRUE`
#'
#' @seealso [use_cassette()], [eject_cassette()]
#'
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = "~/fixtures/vcr_cassettes")
#'
#' (x <- insert_cassette(name = "leo5"))
#' cassette_current()
#' x$new_recorded_interactions
#' cli <- crul::HttpClient$new(url = "https://httpbin.org")
#' cli$get("get")
#' x$new_recorded_interactions
#' # very important when using inject_cassette: use eject cassette when finished
#' x$eject()
#' eject_cassette("leo5") # same as eject_cassette()
#' }
insert_cassette <- function(name, record="once", match_requests_on=NULL,
  re_record_interval=NULL, tag=NULL, tags=NULL,
  update_content_length_header=FALSE, decode_compressed_response=FALSE,
  allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE,
  exclusive=FALSE, serialize_with="yaml", persist_with="FileSystem",
  preserve_exact_body_bytes=TRUE, ignore_cassettes = TRUE) {

  # enable webmockr
  webmockr::enable()

  if (turned_on()) {
    if ( any( name %in% names(cassettes_session()) ) ) {
      stop(sprintf("There is already a cassette with the same name: %s", name),
           call. = FALSE)
    }

    tmp <- Cassette$new(
      name = name, record = record, match_requests_on = match_requests_on,
      re_record_interval = re_record_interval, tag = tag, tags = tags,
      update_content_length_header = update_content_length_header,
      decode_compressed_response = decode_compressed_response,
      allow_playback_repeats = allow_playback_repeats,
      allow_unused_http_interactions = allow_unused_http_interactions,
      exclusive = exclusive,
      serialize_with = serialize_with, persist_with = persist_with,
      preserve_exact_body_bytes = preserve_exact_body_bytes)
    webmockr::webmockr_allow_net_connect()
    #webmockr::webmockr_disable_net_connect()
    return(tmp)
  } else {
    if (ignore_cassettes) {
      message <- "VCR is turned off.  You must turn it on before you can insert a cassette.
      Or you can use the ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message, call. = FALSE)
    }
  }
}
