vcr__env <- new.env()

#' Insert a cassette to record HTTP requests
#'
#' @export
#' @inheritParams use_cassette
#' @param ignore_cassettes (logical) turn \pkg{vcr} off and ignore cassette
#' insertions (so that no error is raised). Default: `FALSE`
#' @seealso [use_cassette()], [eject_cassette()]
#' @return an object of class `Cassette`
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir())
#' webmockr::webmockr_allow_net_connect()
#'
#' (x <- insert_cassette(name = "leo5"))
#' current_cassette()
#' x$new_recorded_interactions
#' cli <- crul::HttpClient$new(url = "https://httpbin.org")
#' cli$get("get")
#' x$new_recorded_interactions
#' # very important when using inject_cassette: eject when done
#' x$eject() # same as eject_cassette("leo5")
#'
#' # cleanup
#' unlink(file.path(tempdir(), "leo5.yml"))
#' }
insert_cassette <- function(name, record="once",
  match_requests_on = c('method', 'uri'),
  update_content_length_header=FALSE,
  allow_playback_repeats=FALSE, serialize_with="yaml",
  persist_with="FileSystem",
  preserve_exact_body_bytes=FALSE, ignore_cassettes = FALSE, 
  re_record_interval = NULL, clean_outdated_http_interactions = FALSE) {

  if (turned_on()) {
    if ( any( name %in% names(cassettes_session()) ) ) {
      stop(sprintf("There is already a cassette with the same name: %s", name),
           "\n  see ?eject_cassette")
    }

    # enable webmockr
    webmockr::enable()
    # FIXME: temporary attempt to make it work: turn on mocking for httr
    # webmockr::httr_mock()

    # record cassete name for use in logging, etc.
    vcr__env$current_cassette <- name

    # make cassette
    tmp <- Cassette$new(
      name = name, record = record, match_requests_on = match_requests_on,
      re_record_interval = re_record_interval, 
      clean_outdated_http_interactions = clean_outdated_http_interactions,
      tag = NULL, tags = NULL,
      update_content_length_header = update_content_length_header,
      decode_compressed_response = NULL,
      allow_playback_repeats = allow_playback_repeats,
      allow_unused_http_interactions = NULL,
      exclusive = NULL,
      serialize_with = serialize_with, persist_with = persist_with,
      preserve_exact_body_bytes = preserve_exact_body_bytes)
    return(tmp)
  } else if (ignore_cassettes) {
    message <- "vcr is turned off.  You must turn it on before you can insert a cassette.
      Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions."
    stop(message)
  } else {
    # vcr is turned off and `ignore_cassettes=TRUE`, returns NULL
    return(NULL)
  }
}
