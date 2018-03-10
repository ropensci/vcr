#' Insert a cassette to record HTTP requests
#'
#' @export
#' @inheritParams use_cassette
#' @param ignore_cassettes (logical) turn \pkg{vcr} off and ignore cassette 
#' insertions (so that no error is raised). Default: `FALSE`
#' @seealso [use_cassette()], [eject_cassette()]
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
#' # very important when using inject_cassette: eject when done
#' x$eject() # same as eject_cassette("leo5")
#' }
insert_cassette <- function(name, record="once", match_requests_on=NULL,
  re_record_interval=NULL, tag=NULL, tags=NULL,
  update_content_length_header=FALSE, decode_compressed_response=FALSE,
  allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE,
  exclusive=FALSE, serialize_with="yaml", persist_with="FileSystem",
  preserve_exact_body_bytes=TRUE, ignore_cassettes = FALSE) {

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
    return(tmp)
  } else {
    if (ignore_cassettes) {
      message <- "vcr is turned off.  You must turn it on before you can insert a cassette.
      Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message, call. = FALSE)
    }
  }
}
