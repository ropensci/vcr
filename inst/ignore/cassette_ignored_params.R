#' Cassette handler
#'
#' @param re_record_interval (integer) When given, the
#' cassette will be re-recorded at the given interval, in seconds.
#' IGNORED FOR NOW.
#' @param tag (character) Used to apply tagged `before_record`
#' and `before_playback` hooks to the cassette. IGNORED FOR NOW.
#' @param tags Used to apply multiple tags to
#' a cassette so that tagged `before_record` and `before_playback` hooks
#' will apply to the cassette. IGNORED FOR NOW.
#' @param decode_compressed_response (logical) Whether or
#' not to decode compressed responses before recording the cassette.
#' This makes the cassette more human readable. Default: `FALSE`.
#' IGNORED FOR NOW.
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
#' @param clean_outdated_http_interactions (logical) Should outdated
#' interactions be recorded back to file. Default: `FALSE`. IGNORED FOR NOW.

#' Use a cassette
#'
#' @param re_record_interval (integer) When given, the
#' cassette will be re-recorded at the given interval, in seconds.
#' IGNORED FOR NOW.
#' @param tag (character) Used to apply tagged `before_record`
#' and `before_playback` hooks to the cassette. IGNORED FOR NOW.
#' @param tags Used to apply multiple tags to
#' a cassette so that tagged `before_record` and `before_playback` hooks
#' will apply to the cassette. IGNORED FOR NOW.
#' @param decode_compressed_response (logical) Whether or
#' not to decode compressed responses before recording the cassette.
#' This makes the cassette more human readable. Default: `FALSE`.
#' IGNORED FOR NOW.
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
