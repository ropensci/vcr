#' vcr: Record HTTP Calls to Disk
#'
#' \pkg{vcr} records test suite 'HTTP' requests and replay them during
#' future runs.
#'
#' Check out the [http testing book](https://books.ropensci.org/http-testing/)
#' for a lot more documentation on `vcr`, `webmockr`, and `crul`
#'
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
#' @section Record modes:
#' See [recording] for help on record modes.
#'
#' @section Request matching:
#' See [request-matching] for help on the many request matching options.
#'
#' @importFrom R6 R6Class
#' @importFrom utils getParseData
#' @importFrom yaml yaml.load yaml.load_file as.yaml
#' @importFrom base64enc base64decode base64encode
#' @importFrom urltools url_parse url_compose
#' @importFrom crul HttpClient mock
#' @importFrom httr http_status content
#' @importFrom webmockr pluck_body
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @docType package
#' @aliases vcr-package
#' @name vcr
NULL

#' An HTTP request as prepared by the \pkg{crul} package
#'
#' The object is a list, and is the object that is passed on to
#' \pkg{webmockr} and \pkg{vcr} instead of routing through
#' \pkg{crul} as normal. Used in examples/tests.
#'
#' @format A list
#' @name crul_request
#' @docType data
#' @keywords data
NULL
