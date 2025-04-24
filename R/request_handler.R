#' @title RequestHandler
#' @description Base handler for http requests, deciding whether a
#' request is stubbed, to be ignored, recordable, or unhandled
#' @export
#' @details
#' \strong{Private Methods}
#'   \describe{
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
RequestHandler <- R6::R6Class(
  'RequestHandler',
  public = list(
    #' @field request_original original, before any modification
    request_original = NULL,
    #' @field request the request, after any modification
    request = NULL,
    #' @field stubbed_response the stubbed response
    stubbed_response = NULL,

    #' @description Create a new `RequestHandler` object
    #' @param request A request
    #' @return A new `RequestHandler` object
    initialize = function(request) {
      self$request_original <- request
      self$request <- {
        Request$new(
          request$method,
          request$url$url %||% request$url,
          take_body(request) %||% "",
          request$headers,
          disk = !is.null(request$output$path)
        )
      }
    },

    #' @description Handle the request (`request` given in `$initialize()`)
    #' @return handles a request, outcomes vary
    handle = function() {
      vcr_log_sprintf(
        "Handling request: %s (disabled: %s)",
        private$request_summary(self$request),
        private$is_disabled()
      )

      if (private$externally_stubbed()) {
        # FIXME: not quite sure what externally_stubbed is meant for
        #   perhaps we can get rid of it here if only applicable in Ruby
        vcr_log_sprintf("- externally stubbed")
        private$on_externally_stubbed_request()
      } else if (private$should_ignore(self$request)) {
        vcr_log_sprintf("- ignored")
        private$on_ignored_request()
      } else if (private$has_response_stub(self$request)) {
        vcr_log_sprintf("- stubbed by vcr")
        private$on_stubbed_by_vcr_request()
      } else if (real_http_connections_allowed()) {
        vcr_log_sprintf("- recordable")
        private$on_recordable_request()
      } else {
        vcr_log_sprintf("- unhandled")
        private$on_unhandled_request()
      }
    }
  ),

  private = list(
    request_summary = function(request) {
      request_matchers <- current_cassette()$match_requests_on
      request_summary(request, request_matchers)
    },

    # request type helpers
    externally_stubbed = function() FALSE,
    should_ignore = function(request) {
      should_be_ignored(request)
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

    on_externally_stubbed_request = function() NULL,
    on_ignored_request = function() {
      # perform and return REAL http response
      # reassign per adapter
    },
    on_stubbed_by_vcr_request = function() {
      # return stubbed vcr response - no real response to do
      # reassign per adapter
    },
    on_recordable_request = function() {
      # do real request - then stub response - then return stubbed vcr response
      # - this may need to be called from webmockr cruladapter?
      # reassign per adapter
    },
    on_unhandled_request = function() {
      err <- UnhandledHTTPRequestError$new(self$request)
      err$run()
    }
  )
)
