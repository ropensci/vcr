#' Manually insert and eject a cassette
#'
#' Generally you should not need to use these functions, instead preferring
#' [use_cassette()] or [local_cassette()]
#'
#' @export
#' @inheritParams use_cassette
#' @inheritSection use_cassette Cassette options
#' @return A [Cassette], invisibly.
#' @order 1
#' @keywords internal
insert_cassette <- function(
  name,
  dir = NULL,
  record = NULL,
  match_requests_on = NULL,
  allow_playback_repeats = FALSE,
  serialize_with = NULL,
  preserve_exact_body_bytes = NULL,
  re_record_interval = NULL,
  clean_outdated_http_interactions = NULL
) {
  if (!turned_on()) {
    if (!the$light_switch$ignore_cassettes) {
      message <- "vcr is turned off.  You must turn it on before you can insert a cassette.
        Or you can set ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message)
    } else {
      # vcr is turned off and `ignore_cassettes=TRUE`, returns NULL
      return(NULL)
    }
  }
  # enable webmockr
  webmockr::enable(quiet = TRUE)
  suppressMessages(webmockr::webmockr_allow_net_connect())

  # make cassette
  cassette <- Cassette$new(
    name,
    dir = dir,
    record = record,
    match_requests_on = match_requests_on,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes,
    re_record_interval = re_record_interval,
    clean_outdated_http_interactions = clean_outdated_http_interactions
  )
  cassette_push(cassette)
  cassette$insert()

  invisible(cassette)
}
