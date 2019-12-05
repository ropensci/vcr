#' @title RequestHandler
#' @description Base handler for http requests, deciding whether a
#' request is stubbed, to be ignored, recordable, or unhandled
#' @export
#' @details
#' \strong{Private Methods}
#'   \describe{
#'     \item{\code{request_type(request)}}{
#'       Get the request type
#'     }
#'     \item{\code{externally_stubbed()}}{
#'       just returns FALSE
#'     }
#'     \item{\code{should_ignore()}}{
#'       should we ignore the request, depends on request ignorer
#'       infrastructure that's not working yet
#'     }
#'     \item{\code{has_response_stub()}}{
#'       Check if there is a matching response stub in the
#'       http interaction list
#'     }
#'     \item{\code{get_stubbed_response()}}{
#'       Check for a response and get it
#'     }
#'     \item{\code{request_summary(request)}}{
#'       get a request summary
#'     }
#'     \item{\code{on_externally_stubbed_request(request)}}{
#'       on externally stubbed request do nothing
#'     }
#'     \item{\code{on_ignored_request(request)}}{
#'       on ignored request, do something
#'     }
#'     \item{\code{on_recordable_request(request)}}{
#'       on recordable request, record the request
#'     }
#'     \item{\code{on_unhandled_request(request)}}{
#'       on unhandled request, run UnhandledHTTPRequestError
#'     }
#'   }
#' @examples \dontrun{
#' # record mode: once
#' vcr_configure(
#'  dir = tempdir(),
#'  record = "once"
#' )
#'
#' data(crul_request)
#' crul_request$url$handle <- curl::new_handle()
#' crul_request
#' x <- RequestHandler$new(crul_request)
#' # x$handle()
#'
#' # record mode: none
#' vcr_configure(
#'  dir = tempdir(),
#'  record = "none"
#' )
#' data(crul_request)
#' crul_request$url$handle <- curl::new_handle()
#' crul_request
#' insert_cassette("testing_record_mode_none", record = "none")
#' file.path(vcr_c$dir, "testing_record_mode_none.yml")
#' x <- RequestHandlerCrul$new(crul_request)
#' # x$handle()
#' crul_request$url$url <- "https://api.crossref.org/works/10.1039/c8sm90002g/"
#' crul_request$url$handle <- curl::new_handle()
#' z <- RequestHandlerCrul$new(crul_request)
#' # z$handle()
#' eject_cassette("testing_record_mode_none")
#' }
RequestHandler <- R6::R6Class(
  'RequestHandler',
  public = list(
    #' @field request_original original, before any modification
    request_original = NULL,
    #' @field request the request, after any modification
    request = NULL,
    #' @field vcr_response holds [VcrResponse] object
    vcr_response = NULL,
    #' @field stubbed_response the stubbed response
    stubbed_response = NULL,
    #' @field cassette the cassette holder
    cassette = NULL,

    #' @description Create a new `RequestHandler` object
    #' @param request The request from an object of class `HttpInteraction`
    #' @return A new `RequestHandler` object
    initialize = function(request) {
      self$request_original <- request
      self$request <- {
        Request$new(request$method, request$url$url %||% request$url,
          pluck_body(request), request$headers,
          disk = !is.null(request$output$path))
      }
      self$cassette <- tryCatch(current_cassette(), error = function(e) e)
    },

    #' @description Handle the request (`request` given in `$initialize()`)
    #' @return handles a request, outcomes vary
    handle = function() {
      vcr_log_info(sprintf("Handling request: %s (disabled: %s)",
        private$request_summary(self$request),
        private$is_disabled()), vcr_c$log_opts$date)

      req_type <- private$request_type()
      req_type_fun <- sprintf("private$on_%s_request", req_type)

      vcr_log_info(sprintf("Identified request type: (%s) for %s",
        req_type,
        private$request_summary(self$request)), vcr_c$log_opts$date)

      eval(parse(text = req_type_fun))(self$request)
    }
  ),

  private = list(
    request_summary = function(request) {
      request_matchers <- if (
        !inherits(self$cassette, c("error", "list")) &&
        !is.null(self$cassette)
      ) {
        self$cassette$match_requests_on
      } else {
        vcr_c$match_requests_on
      }
      request_summary(Request$new()$from_hash(request), request_matchers)
    },

    request_type = function() {
      if (private$externally_stubbed()) {
        # FIXME: not quite sure what externally_stubbed is meant for
        #   perhaps we can get rid of it here if only applicable in Ruby
        # cat("request_type: is externally stubbed", "\n")
        "externally_stubbed"
      } else if (private$should_ignore(self$request)) {
        # cat("request_type: is ignored", "\n")
        "ignored"
      } else if (private$has_response_stub(self$request)) {
        # cat("request_type: is stubbed_by_vcr", "\n")
        "stubbed_by_vcr"
      } else if (real_http_connections_allowed()) {
        # cat("request_type: is recordable", "\n")
        "recordable"
      } else {
        # cat("request_type: is unhandled", "\n")
        "unhandled"
      }
    },

    # request type helpers
    externally_stubbed = function() FALSE,
    should_ignore = function(request) {
      request_ignorer$should_be_ignored(request)
    },
    has_response_stub = function(request) {
      hi <- http_interactions()
      if (length(hi$interactions) == 0) return(FALSE)
      hi$has_interaction_matching(request)
    },
    is_disabled = function(adapter = "crul") !webmockr::enabled(adapter),


    # get stubbed response
    get_stubbed_response = function(request) {
      self$stubbed_response <- http_interactions()$response_for(request)
      self$stubbed_response
    },

    #####################################################################
    ### various on* methods, some global for any adapter,
    ###   and some may be specific to an adapter
    ###   - all fxns take `request` param for consistentcy, even if they dont use it
    ##### so we can "monkey patch" these in each HTTP client adapter by
    #####   reassigning some of these functions with ones specific to the HTTP client

    on_externally_stubbed_request = function(request) NULL,
    on_ignored_request = function(request) {
      # perform and return REAL http response
      # reassign per adapter
    },
    on_stubbed_by_vcr_request = function(request) {
      # return stubbed vcr response - no real response to do
      # reassign per adapter
    },
    on_recordable_request = function(request) {
      # do real request - then stub response - then return stubbed vcr response
      # - this may need to be called from webmockr cruladapter?
      # reassign per adapter
    },
    on_unhandled_request = function(request) {
      err <- UnhandledHTTPRequestError$new(request)
      err$run()
    }
  )
)

test_if_not_null <- function(x) {
  if (!is.null(x)) x$options$postfieldsize == 0
}

# try to figure out where the body is located
# either:
# - $fields as string or list
# - $fields as an upload
# - $options$postfields as raw
#
# return: character string
pluck_body <- function(x) {
  if (
    is.null(x$fields) && {
      if (is.null(x$options$postfieldsize)) return(FALSE)
      x$options$postfieldsize == 0
    }
  ) {
    return(NULL)
  }
  if (!is.null(x$fields)) {
    form_file_comp <- vapply(x$fields, inherits, logical(1), "form_file")
    if (any(form_file_comp)) {
      ff <- x$fields[form_file_comp][[1]]
      return(sprintf("type=%s; path=%s", ff$type, ff$path))
    } else {
      return(x$fields)
    }
  }
  if (!is.null(x$options$postfields)) {
    if (is.raw(x$options$postfields)) return(rawToChar(x$options$postfields))
  }
  stop("couldn't fetch body; file an issue at \n",
    "  https://github.com/ropensci/vcr/issues/")
}
