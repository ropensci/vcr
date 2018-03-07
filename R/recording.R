#' vcr recording options
#'
#' @section once:
#' The `once` record mode will:
#'
#' - Replay previously recorded interactions.
#' - Record new interactions if there is no cassette file.
#' - Cause an error to be raised for new requests if there is a cassette file.
#'
#'
#' It is similar to the `new_episodes` record mode, but will prevent new,
#' unexpected requests from being made (i.e. because the request URI changed
#' or whatever).
#'
#' `once` is the default record mode, used when you do not set one.
#'
#' @section none:
#' The `none` record mode will:
#'
#' - Replay previously recorded interactions.
#' - Cause an error to be raised for any new requests.
#'
#'
#' This is useful when your code makes potentially dangerous
#' HTTP requests.  The `none` record mode guarantees that no
#' new HTTP requests will be made.
#'
#' @section new_episodes:
#' The `new_episodes` record mode will:
#'
#' - Record new interactions.
#' - Replay previously recorded interactions.
#'
#'
#' It is similar to the `once` record mode, but will **always** record new
#' interactions, even if you have an existing recorded one that is similar
#' (but not identical, based on the `match_request_on` option).
#'
#' @section all:
#' The `all` record mode will:
#'
#' - Record new interactions.
#' - Never replay previously recorded interactions.
#'
#'
#' This can be temporarily used to force VCR to re-record
#' a cassette (i.e. to ensure the responses are not out of date)
#' or can be used when you simply want to log all HTTP requests.
#'
#' @name recording
NULL
