#' @section Backstory:
#' A Ruby gem of the same name (`VCR`, <https://github.com/vcr/vcr>) was
#' created many years ago and is the original. Ports in many languages
#' have been done. Check out that GitHub repo for all the details on
#' how the canonical version works.
#'
#' @section Main functions:
#' The [use_cassette] function is most likely what you'll want to use. It
#' sets the cassette you want to record to, inserts the cassette, and then
#' ejects the cassette, recording the interactions to the cassette.
#'
#' Instead, you can use [insert_cassette], but then you have to make sure
#' to use [eject_cassette].
#'
#' @section vcr configuration:
#' [vcr_configure] is the function to use to set R session wide settings.
#' See it's manual file for help.
#'
#' @section Async:
#' As of \pkg{crul} v1.5, `vcr` will work for async http requests with
#' \pkg{crul}. \pkg{httr} does not do async requests, and \pkg{httr2}
#' async plumbing does not have any hooks for mocking via \pkg{webmockr}
#' or recording real requests via this package
#'
#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import rlang
#' @importFrom R6 R6Class
#' @importFrom utils getParseData
#' @importFrom yaml yaml.load yaml.load_file as.yaml
#' @importFrom urltools url_parse url_compose
## usethis namespace: end
NULL
