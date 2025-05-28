Interactions <- R6::R6Class(
  'Interactions',
  public = list(
    interactions = NULL,
    replayable = logical(),

    request_matchers = NULL,

    initialize = function(
      interactions = list(),
      request_matchers = c("method", "uri")
    ) {
      self$interactions <- interactions
      self$replayable <- rep(TRUE, length(interactions))

      self$request_matchers <- request_matchers
    },

    # Returns index; powers all other methods
    find_request = function(request, allow_playback = FALSE) {
      the$last_request <- encode_request(
        request,
        matchers = self$request_matchers
      )

      for (i in seq_along(self$interactions)) {
        if (!self$replayable[[i]] && !allow_playback) {
          next
        }

        request_i <- self$interactions[[i]]$request
        if (request_matches(request, request_i, self$request_matchers, i)) {
          the$last_response <- encode_response(self$interactions[[i]]$response)
          return(i)
        }
      }

      the$last_response <- NULL
      return(NA_integer_)
    },

    add = function(request, response) {
      the$last_request <- encode_request(
        request,
        matchers = self$request_matchers
      )
      the$last_response <- encode_response(response)
      interaction <- vcr_interaction(request, response)

      idx <- length(self$interactions) + 1
      self$interactions[[idx]] <- interaction
      self$replayable[[idx]] <- FALSE # don't allow playback for new interactions

      interaction
    },

    # Returns response
    replay_request = function(i) {
      self$replayable[[i]] <- FALSE
      self$interactions[[i]]$response
    },

    has_interaction = function(request) {
      idx <- self$find_request(request)
      !is.na(idx)
    },

    n_replayable = function() {
      sum(self$replayable)
    },

    length = function() {
      length(self$interactions)
    }
  )
)

#' Retrieve last vcr request/response
#'
#' When debugging, it's often useful to see the last request and response
#' respectively. These functions give you what would have been recorded to disk
#' or replayed from disk. If the last request wasn't recorded, and there was
#' no matching request, `vcr_last_response` will return `NULL`.
#'
#' @export
vcr_last_request <- function() {
  the$last_request
}

#' @export
#' @rdname vcr_last_request
vcr_last_response <- function() {
  the$last_response
}
