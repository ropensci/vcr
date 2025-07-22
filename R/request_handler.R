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
      matchers <- current_cassette()$match_requests_on
      if (identical(matchers, "default")) {
        matchers <- default_matcher(self$request)
      }
      summary <- request_summary(self$request, matchers)
      vcr_log_sprintf("Handling request: %s", summary)

      if (should_be_ignored(self$request)) {
        vcr_log_sprintf("  ignored")
        return(self$on_ignored_request())
      }

      if (current_cassette_replaying()) {
        vcr_log_sprintf(
          "  Looking for existing requests using %s",
          paste0(matchers, collapse = "/")
        )

        cassette <- current_cassette()
        interactions <- cassette$http_interactions
        idx <- interactions$find_request(self$request)
        if (!is.na(idx)) {
          vcr_response <- interactions$replay_request(idx)
          vcr_log_sprintf("  Replaying response %i", idx)
          return(self$on_stubbed_by_vcr_request(vcr_response))
        } else {
          vcr_log_sprintf("  No matching requests")
        }
      }

      if (current_cassette_recording()) {
        return(self$on_recordable_request())
      }

      # Since it's going to error, there's no point in also giving a warning
      # about the cassette being empty
      if (cassette_active()) {
        cassette <- current_cassette()
        cassette$warn_on_empty <- FALSE
      }
      cli::cli_abort(
        c(
          "Failed to find matching request in active cassette, {.str {cassette$name}}.",
          i = if (!the$config$log) {
            "Use {.fn local_vcr_configure_log} to get more details."
          },
          i = "Learn more in {.vignette vcr::debugging}."
        ),
        class = "vcr_unhandled"
      )
    },

    # The following three methods are overriden by subclasses, providing
    # the specifics of request handling for different packages
    on_ignored_request = function() {
      # Perform real request and return response
    },
    on_stubbed_by_vcr_request = function() {
      # Return stubbed response
    },
    on_recordable_request = function() {
      # Perform real request, save response as stub, return reponse
    }
  )
)
