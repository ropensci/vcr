#' Use a cassette to record an HTTP call
#'
#' @export
#' @examples \dontrun{
#' res <- Cassette$new("foobar")
#'
#' x <- cassettes()
#' (cas <- as.cassette(x[[1]]))
#' as.cassette(cas)
#' as.cassette(cassettes()[[1]])
#' as.cassette("foobar")
#'
#' insert_cassette(name = "fartloud")
#'
#' use_cassette("foobar", GET("http://google.com"))
#' }

use_cassette <- function(name, ..., record="once", match_requests_on=NULL, re_record_interval=NULL,
  tag=NULL, tags=NULL, update_content_length_header=FALSE, decode_compressed_response=FALSE,
  allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE, exclusive=FALSE,
  serialize_with="yaml", persist_with="FileSystem", preserve_exact_body_bytes=TRUE) {

  #if (block) stop(errmssg, call. = FALSE)
  cassette <- if (!cassette_exists(name)) insert_cassette(name) else name
  call_block(cassette, ...)
  # eject_cassette(cassette)
}

#' @export
#' @rdname use_cassette
insert_cassette <- function(name, record="once", match_requests_on=NULL, re_record_interval=NULL,
  tag=NULL, tags=NULL, update_content_length_header=FALSE, decode_compressed_response=FALSE,
  allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE, exclusive=FALSE,
  serialize_with="yaml", persist_with="FileSystem", preserve_exact_body_bytes=TRUE,
  ignore_cassettes = TRUE) {

  if (turned_on()) {
    if ( any( name %in% names(cassettes()) ) ) {
      stop(sprintf("There is already a cassette with the same name: %s", name), call. = FALSE)
    }

    Cassette$new(name = name, record = record, match_requests_on = match_requests_on,
                 re_record_interval = re_record_interval, tag = tag, tags = tags,
                 update_content_length_header = update_content_length_header,
                 decode_compressed_response = decode_compressed_response,
                 allow_playback_repeats = allow_playback_repeats,
                 allow_unused_http_interactions = allow_unused_http_interactions,
                 exclusive = exclusive,
                 serialize_with = serialize_with, persist_with = persist_with,
                 preserve_exact_body_bytes = preserve_exact_body_bytes)
    # cassettes.push(cassette)
  } else {
    if (ignore_cassettes) {
      message <- "VCR is turned off.  You must turn it on before you can insert a cassette.
      Or you can use the ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message, call. = FALSE)
    }
  }
}

#' @export
#' @rdname use_cassette
eject_cassette <- function(cassettes, options = list()) {
  # Cassette$eject()
  cassette <- last(cassettes())
  # cassette.eject(options) # if cassette use cassette_eject()
  # cassette
  # ensure
  #   cassettes.pop
}
