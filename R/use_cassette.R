#' Use a cassette
#'
#' @export
#' @param name The name of the cassette. vcr will sanitize this to ensure it
#' is a valid file name.
#' @param ... a block of code to evalulate, wrapped in curly braces. required.
#' if you don't pass a code block you'll get a stop message. if you can't pass
#' a code block use instead [insert_cassette()]
#' @param record The record mode. Default: "once". In the future we'll support
#' "once", "all", "none", "new_episodes". See [recording] for more information
#' @param match_requests_on List of request matchers
#' to use to determine what recorded HTTP interaction to replay. Defaults to
#' `["method", "uri"]`. The built-in matchers are "method", "uri", "host",
#' "path", "headers" and "body"
#' @param update_content_length_header (logical) Whether or
#' not to overwrite the `Content-Length` header of the responses to
#' match the length of the response body. Default: `FALSE`
#' @param allow_playback_repeats (logical) Whether or not to
#' allow a single HTTP interaction to be played back multiple times.
#' Default: `FALSE`.
#' @param serialize_with (character) Which serializer to use.
#'  Valid values are "yaml" (default), the only one supported for now.
#' @param persist_with (character) Which cassette persister to
#'  use. Default: "file_system". You can also register and use a
#'  custom persister.
#' @param preserve_exact_body_bytes (logical) Whether or not
#' to base64 encode the bytes of the requests and responses for
#' this cassette when serializing it. See also `preserve_exact_body_bytes`
#' in [vcr_configure()]. Default: `FALSE`
#'
#' @details A run down of the family of top level \pkg{vcr} functions
#'
#' - `use_cassette` Initializes a cassette. Returns the inserted
#'  cassette.
#' - `insert_cassette` Internally used within `use_cassette`
#' - `eject_cassette` ejects the current cassette. The cassette
#'  will no longer be used. In addition, any newly recorded HTTP interactions
#'  will be written to disk.
#'
#' @section Behavior:
#' This function handles a few different scenarios:
#'
#' - when everything runs smoothly, and we return a `Cassette` class object
#' so you can inspect the cassette, and the cassette is ejected
#' - when there is an invalid parameter input on cassette creation,
#' we fail with a useful message, we don't return a cassette, and the
#' cassette is ejected
#' - when there is an error in calling your passed in code block,
#' we return with a useful message, and since we use `on.exit()`
#' the cassette is still ejected even though there was an error,
#' but you don't get an object back
#'
#' @section Cassettes on disk:
#' Note that _"eject"_ only means that the R sesion cassette is no longer
#' in use. If any interactions were recorded to disk, then there is a file
#' on disk with those interactions.
#'
#' @return an object of class `Cassette`
#'
#' @seealso [insert_cassette()], [eject_cassette()]
#' @examples \dontrun{
#' library(vcr)
#' library(crul)
#' vcr_configure(dir = tempdir())
#'
#' use_cassette(name = "apple7", {
#'   cli <- HttpClient$new(url = "https://httpbin.org")
#'   resp <- cli$get("get")
#' })
#' readLines(file.path(tempdir(), "apple7.yml"))
#'
#' # preserve exact body bytes - records in base64 encoding
#' use_cassette("things4", {
#'   cli <- crul::HttpClient$new(url = "https://httpbin.org")
#'   bbb <- cli$get("get")
#' }, preserve_exact_body_bytes = TRUE)
#' ## see the body string value in the output here
#' readLines(file.path(tempdir(), "things4.yml"))
#'
#' # cleanup
#' unlink(file.path(tempdir(), c("things4.yml", "apple7.yml")))
#' }

use_cassette <- function(name, ..., record = "once",
  match_requests_on = c("method", "uri"),
  update_content_length_header = FALSE,
  allow_playback_repeats = FALSE,
  serialize_with = "yaml", persist_with = "FileSystem",
  preserve_exact_body_bytes = FALSE) {

  cassette <- insert_cassette(
    name, record = record, match_requests_on = match_requests_on,
    update_content_length_header = update_content_length_header,
    allow_playback_repeats = allow_playback_repeats,
    serialize_with = serialize_with, persist_with = persist_with,
    preserve_exact_body_bytes = preserve_exact_body_bytes)
  on.exit(cassette$eject())
  cassette$call_block(...)
  return(cassette)
}
