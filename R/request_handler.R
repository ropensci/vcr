#' Request handlers
#'
#' These are internal classes that should not be used by users.
#'
#' @keywords internal
#' @aliases RequestHandlerHttr2 RequestHandlerHttr RequestHandlerCrul
#' @name RequestHandler
NULL

RequestHandler <- R6::R6Class(
  'RequestHandler',
  public = list(
    # request_original Request from the HTTP package.
    request_original = NULL,
    # request A `vcr_request` object
    request = NULL,

    handle = function() {
      vcr_log_sprintf(
        "Handling request: %s",
        private$request_summary(self$request)
      )

      if (private$externally_stubbed()) {
        # FIXME: not quite sure what externally_stubbed is meant for
        #   perhaps we can get rid of it here if only applicable in Ruby
        vcr_log_sprintf("  externally stubbed")
        return(private$on_externally_stubbed_request())
      }

      if (should_be_ignored(self$request)) {
        vcr_log_sprintf("  ignored")
        return(private$on_ignored_request())
      }

      if (cassette_active()) {
        interactions <- current_cassette()$http_interactions
        vcr_log_sprintf(
          "  looking for existing requests using %s",
          paste0(interactions$request_matchers, collapse = "/")
        )
        idx <- interactions$find_request(self$request)
        if (!is.na(idx)) {
          vcr_response <- interactions$response_for(idx)
          vcr_log_sprintf("  matched response %i", idx)
          return(private$on_stubbed_by_vcr_request(vcr_response))
        } else {
          vcr_log_sprintf("  no matching requests")
        }
      }

      if (cassette_is_recording()) {
        return(private$on_recordable_request())
      }

      err <- UnhandledHTTPRequestError$new(self$request)
      err$run()
    }
  ),

  private = list(
    request_summary = function(request) {
      request_matchers <- current_cassette()$match_requests_on
      request_summary(request, request_matchers)
    },

    # request type helpers
    externally_stubbed = function() FALSE,

    get_stubbed_response = function(request) {
      if (!cassette_active()) {
        return(NULL)
      }
      interactions <- current_cassette()$http_interactions
      interactions$response_for(request)
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
    }
  )
)

cassette_is_recording <- function() {
  if (cassette_active()) {
    current_cassette()$recording()
  } else {
    FALSE
  }
}

cassette_has_response <- function(request) {
  if (cassette_active()) {
    interactions <- current_cassette()$http_interactions
    interactions$has_interaction(request)
  } else {
    FALSE
  }
}
