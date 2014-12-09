#' Use cassette
#'
#' @export
#'
#' @param name The name of the cassette. vcr will sanitize this to ensure it is a valid file name.
#' @param record The record mode. One of "all", "none", "new_episodes", "once". See Details.
#' @param match_requests_on List of request matchers
#'  to use to determine what recorded HTTP interaction to replay. Defaults to
#'  ("method", "uri"). The built-in matchers are "method", "uri", "host", "path", "headers"
#'  and "body". You can also pass the name of a registered custom request matcher or
#'  any object that responds to #call.
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
#'
#' @details
#' \itemize{
#'  \item \code{use_cassette} Initialize a cassett. Returns the inserted cassette.
#'  \item \code{insert_cassette} Internally used within \code{use_cassette}.
#'  \item \code{eject_cassette} ejects the current cassette. The cassette will no longer be used.
#'  In addition, any newly recorded HTTP interactions will be written to disk.
#' }
#'
#' Options for \code{record} parameter:
#' \itemize{
#'  \item all - Record every HTTP interactions; do not play any back.
#'  \item none - Do not record any HTTP interactions; play them back.
#'  \item new_episodes - Playback previously recorded HTTP interactions and record new ones.
#'  \item once - Record the HTTP interactions if the cassette has not already been recorded;
#'  otherwise, playback the HTTP interactions.
#' }
#'
#' @note If you use this method you \emph{must} call \code{eject_cassette()} when you
#'  are done.
#'
#' @examples \dontrun{
#' insert_cassette(name = "foobar")
#' }

use_cassette <- function(name, record="once", match_requests_on=NULL, re_record_interval=NULL,
  tag=NULL, tags=NULL, update_content_length_header=FALSE, decode_compressed_response=FALSE,
  allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE, exclusive=FALSE,
  serialize_with="yaml", persist_with="file_system", preserve_exact_body_bytes=TRUE,
  block = FALSE)
{
  if(block) stop(errmssg, call. = FALSE)
  cassette <- insert_cassette(name, ...)
  call_block(block, cassette)
  eject_cassette()
}

#' @export
#' @rdname use_cassette
insert_cassette <- function(name, record="once", match_requests_on=NULL, re_record_interval=NULL,
  tag=NULL, tags=NULL, update_content_length_header=FALSE, decode_compressed_response=FALSE,
	allow_playback_repeats=FALSE, allow_unused_http_interactions=TRUE, exclusive=FALSE,
	serialize_with="yaml", persist_with="file_system", preserve_exact_body_bytes=TRUE)
{
  if(turned_on()){
    if( any( name %in% names(cassettes()) ) )
      stop(sprintf("There is already a cassette with the same name: %s", name), call. = FALSE)

    cassette_new(name, record=record, match_requests_on=match_requests_on,
        re_record_interval=re_record_interval, tag=tag, tags=tags,
        update_content_length_header=update_content_length_header,
        decode_compressed_response=decode_compressed_response, allow_playback_repeats=allow_playback_repeats,
        allow_unused_http_interactions=allow_unused_http_interactions, exclusive=exclusive,
        serialize_with=serialize_with, persist_with=persist_with,
        preserve_exact_body_bytes=preserve_exact_body_bytes)
    # cassettes.push(cassette)
  } else {
    if(ignore_cassettes){
      message <- "VCR is turned off.  You must turn it on before you can insert a cassette.
      Or you can use the ignore_cassettes=TRUE option to completely ignore cassette insertions."
      stop(message, call. = FALSE)
    }
  }
}

cassette_new <- function(name, record, match_requests_on, re_record_interval,
  tag, tags, update_content_length_header, decode_compressed_response, allow_playback_repeats,
  allow_unused_http_interactions, exclusive, serialize_with, persist_with, preserve_exact_body_bytes)
{
  args <- list(record=record, match_requests_on=match_requests_on,
               re_record_interval=re_record_interval, tag=tag, tags=tags,
               update_content_length_header=update_content_length_header,
               decode_compressed_response=decode_compressed_response, allow_playback_repeats=allow_playback_repeats,
               allow_unused_http_interactions=allow_unused_http_interactions, exclusive=exclusive,
               serialize_with=serialize_with, persist_with=persist_with,
               preserve_exact_body_bytes=preserve_exact_body_bytes)
  m <- c(name=name, args)
  for(i in seq_along(m)){
    cat(sprintf("%s: %s", names(m[i]), m[i]), file = sprintf("%s/%s_metadata.yml", path.expand(cassette_path()), name), sep = "\n", append = TRUE)
  }
  cat("\n", file = sprintf("%s/%s.yml", path.expand(cassette_path()), name))
  return( structure(metadata, class="cassette") )
}

print.cassette <- function(x, ...){
  cat(paste0("<cassette> ", x$name), sep = "\n")
  cat(paste0("  Record method: ", x$record), sep = "\n")
  cat(paste0("  Serialize with: ", x$serialize_with), sep = "\n")
  cat(paste0("  Persist with: ", x$persist_with), sep = "\n")
  cat(paste0("  update_content_length_header: ", x$update_content_length_header), sep = "\n")
  cat(paste0("  decode_compressed_response: ", x$decode_compressed_response), sep = "\n")
  cat(paste0("  allow_playback_repeats: ", x$allow_playback_repeats), sep = "\n")
  cat(paste0("  allow_unused_http_interactions: ", x$allow_unused_http_interactions), sep = "\n")
  cat(paste0("  exclusive: ", x$exclusive), sep = "\n")
  cat(paste0("  preserve_exact_body_bytes: ", x$preserve_exact_body_bytes), sep = "\n")
}

cassettes <- function(){
  path <- path.expand(cassette_path())
  check_create_path(path)
  list.files(path)
}

cassette_path <- function() '~/vcr/vcr_cassettes'

check_create_path <- function(x){
  if(file.exists(x)) dir.create(x, recursive = TRUE, showWarnings = FALSE)
}

#' @export
#' @rdname use_cassette
#' @param skip_no_unused_interactions_assertion (logical) If \code{TRUE}, this will skip
#' the "no unused HTTP interactions" assertion enabled by the
#' \code{allow_unused_http_interactions=FALSE} cassette option. This is intended for use
#' when your test has had an error, but your test framework has already handled it.
#' @return The ejected cassette if there was one
eject_cassette <- function(cassettes, options = list()){
  cassette <- cassettes.last
  cassette.eject(options) if cassette
  cassette
  ensure
  cassettes.pop
}

#' Turns VCR off for the duration of a block.
#'
#' @export
#' @param options List of options
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#'  inserted while VCR is turned off. If \code{TRUE} is passed, the cassette insertion
#'  will be ignored; otherwise an error will be raised.
#' @seealso turn_off, turn_on, turned_on
turned_off <- function(options = list(), ignore_cassettes = FALSE){
  turn_off!(options)
# begin
# yield
# ensure
# turn_on!
#   end
}

turned_on <- function() TRUE

#' Turns VCR off, so that it no longer handles every HTTP request.
#'
#' @export
#' @param options List of options
#' @param ignore_cassettes (logical) Controls what happens when a cassette is
#'  inserted while VCR is turned off. If \code{TRUE} is passed, the cassette insertion
#'  will be ignored; otherwise an error will be raised.
turn_off <- function(cassette, options = list(), ignore_cassettes = FALSE){
  if( current_cassette ){
    stop("A VCR cassette is currently in use (#{VCR.current_cassette.name}).
         You must eject it before you can turn VCR off.", call. = FALSE)
  }
  if( any(invalid_options(options)) ) stop("You passed some invalid options", call. = FALSE)
  turn_off_()
}

turn_off_ <- function(){
  print("hello world")
}

errmssg <- "use_cassette requires a block.\nIf you cannot wrap your code in a block, use\ninsert_cassette / eject_cassette instead."

# VCR.use_cassette('some_cassette') do
#'    Net::HTTP.get_response(URI('http://example.com/'))
# end

# @raise [ArgumentError] when the given cassette is already being used.
# @raise [VCR::Errors::TurnedOffError] when VCR has been turned off
#  without using the :ignore_cassettes option.
# @raise [VCR::Errors::MissingERBVariableError] when the `:erb` option
#  is used and the ERB template requires variables that you did not provide.
