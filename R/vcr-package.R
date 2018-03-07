#' vcr: Record HTTP Calls to Disk
#'
#' \pkg{vcr} records test suite 'HTTP' requests and replay them during
#' future runs.
#'
#' @section Backstory:
#' A Ruby gem of the same name (`vcr`, <https://github.com/vcr/vcr>) was
#' created many years ago and was the original. Ports in many languages
#' have been done, including this one for R. Check out that github repo
#' for all the details on how the canonical version works.
#'
#' @section Main funcitons:
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
#' @importFrom digest digest
#' @importFrom R6 R6Class
#' @importFrom yaml yaml.load_file
#' @importFrom lazyeval lazy_dots lazy_eval
#' @importFrom base64enc base64decode base64encode
#' @author Scott Chamberlain \email{myrmecocystus@@gmail.com}
#' @docType package
#' @aliases vcr-package
#' @name vcr
NULL
