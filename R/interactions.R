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
      for (i in seq_along(self$interactions)) {
        if (!self$replayable[[i]] && !allow_playback) {
          next
        }

        request_i <- self$interactions[[i]]$request
        if (request_matches(request, request_i, self$request_matchers, i)) {
          return(i)
        }
      }
      return(NA_integer_)
    },

    add = function(request, response) {
      idx <- length(self$interactions) + 1

      interaction <- vcr_interaction(request, response)
      self$interactions[[idx]] <- interaction
      self$replayable[[idx]] <- FALSE # don't allow playback for new interactions

      interaction
    },

    # Returns response
    response_for = function(i) {
      self$replayable[[i]] <- FALSE
      self$interactions[[i]]$response
    },

    has_interaction = function(request) {
      idx <- self$find_request(request)
      !is.na(idx)
    },

    # TODO: can be removed if https://github.com/ropensci/vcr/pull/488 is merged
    has_used_interaction = function(request) {
      idx <- self$find_request(request, allow_playback = TRUE)
      !is.na(idx) && !self$replayable[[idx]]
    },

    n_replayable = function() {
      sum(self$replayable)
    },

    length = function() {
      length(self$interactions)
    }
  )
)
