vcr__env <- new.env()

#' Insert a cassette to record HTTP requests
#'
#' @export
#' @inheritParams use_cassette
#' @inheritSection use_cassette Cassette options
#' @inheritSection check_cassette_names Cassette names
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
#' x$previously_recorded_interactions()
#' cli <- crul::HttpClient$new(url = "https://httpbin.org")
#' cli$get("get")
#' x$new_recorded_interactions # 1 interaction
#' x$previously_recorded_interactions() # empty
#' webmockr::stub_registry() # not empty
#' # very important when using inject_cassette: eject when done
#' x$eject() # same as eject_cassette("leo5")
#' x$new_recorded_interactions # same, 1 interaction
#' x$previously_recorded_interactions() # now not empty
#' ## stub_registry now empty, eject() calls webmockr::disable(), which
#' ## calls the disable method for each of crul and httr adadapters, 
#' ## which calls webmockr's remove_stubs() method for each adapter
#' webmockr::stub_registry()
#'
#' # cleanup
#' unlink(file.path(tempdir(), "leo5.yml"))
#' }
insert_cassette <- function(name,
  record = NULL,
  match_requests_on = NULL,
  update_content_length_header = FALSE,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  persist_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL) {

  check_cassette_name(name)
  vcr_env_handle()
  if (turned_on()) {
    if ( any( name %in% names(cassettes_session()) ) ) {
      stop(sprintf("There is already a cassette with the same name: %s", name),
           "\n  see ?eject_cassette")
    }

    # enable webmockr
    webmockr::enable()
    webmockr::webmockr_allow_net_connect()
    # FIXME: temporary attempt to make it work: turn on mocking for httr
    # webmockr::httr_mock()

    # record cassete name for use in logging, etc.
    vcr__env$current_cassette <- name

    # make cassette
    tmp <- Cassette$new(name,
      record = record %||% vcr_c$record,
      match_requests_on = match_requests_on %||% vcr_c$match_requests_on,
      update_content_length_header = update_content_length_header,
      allow_playback_repeats = allow_playback_repeats,
      serialize_with = serialize_with %||% vcr_c$serialize_with,
      persist_with = persist_with %||% vcr_c$persist_with,
      preserve_exact_body_bytes = preserve_exact_body_bytes %||% vcr_c$preserve_exact_body_bytes,
      re_record_interval = re_record_interval %||% vcr_c$re_record_interval,
      clean_outdated_http_interactions = clean_outdated_http_interactions %||% vcr_c$clean_outdated_http_interactions,
      tag = NULL,
      tags = NULL,
      allow_unused_http_interactions = NULL,
      exclusive = NULL
    )
    return(tmp)
  } else if (!light_switch$ignore_cassettes) {
    message <- "vcr is turned off.  You must turn it on before you can insert a cassette.
      Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions."
    stop(message)
  } else {
    # vcr is turned off and `ignore_cassettes=TRUE`, returns NULL
    return(NULL)
  }
}
