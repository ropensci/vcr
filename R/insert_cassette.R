vcr__env <- new.env()

#' Manually insert and eject a cassette
#'
#' Generally you should not need to use these functions, instead preferring
#' [use_cassette()] or [local_cassette()]
#'
#' @export
#' @inheritParams use_cassette
#' @inheritSection use_cassette Cassette options
#' @inherit check_cassette_names details
#' @return A [Cassette], invisibly.
#' @order 1
#' @keywords internal
insert_cassette <- function(
  name,
  record = NULL,
  match_requests_on = NULL,
  update_content_length_header = FALSE,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  persist_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL
) {
  check_cassette_name(name)

  if (!turned_on()) {
    if (!light_switch$ignore_cassettes) {
      message <- "vcr is turned off.  You must turn it on before you can insert a cassette.
        Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message)
    } else {
      # vcr is turned off and `ignore_cassettes=TRUE`, returns NULL
      return(NULL)
    }
  }

  if (name %in% names(cassettes_session())) {
    stop(
      sprintf("There is already a cassette with the same name: %s", name),
      "\n  see ?eject_cassette"
    )
  }

  # enable webmockr
  webmockr::enable(quiet = vcr_c$quiet)
  sup_mssg(vcr_c$quiet, webmockr::webmockr_allow_net_connect())

  # record cassete name for use in logging, etc.
  vcr__env$current_cassette <- name

  # make cassette
  invisible(Cassette$new(
    name,
    record = record %||% vcr_c$record,
    match_requests_on = match_requests_on %||% vcr_c$match_requests_on,
    update_content_length_header = update_content_length_header,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with %||% vcr_c$serialize_with,
    persist_with = persist_with %||% vcr_c$persist_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes %||%
      vcr_c$preserve_exact_body_bytes,
    re_record_interval = re_record_interval %||% vcr_c$re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions %||%
      vcr_c$clean_outdated_http_interactions,
    tag = NULL,
    tags = NULL,
    allow_unused_http_interactions = NULL,
    exclusive = NULL
  ))
}
