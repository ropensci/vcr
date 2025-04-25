HTTPInteractionList <- R6::R6Class(
  'HTTPInteractionList',
  public = list(
    interactions = NULL,
    used = logical(),

    request_matchers = NULL,
    allow_playback_repeats = FALSE,

    initialize = function(
      interactions,
      request_matchers = c("method", "uri"),
      allow_playback_repeats = FALSE,
      used_interactions = list()
    ) {
      self$interactions <- interactions
      self$used <- rep(FALSE, length(interactions))

      self$request_matchers <- request_matchers
      self$allow_playback_repeats <- allow_playback_repeats
    },

    # Returns index; powers all other methods
    find_request = function(request, allow_playback = NULL) {
      allow_playback <- allow_playback %||% self$allow_playback_repeats

      for (i in seq_along(self$interactions)) {
        if (self$used[[i]] && !allow_playback) {
          next
        }

        request_i <- self$interactions[[i]]$request
        if (request_matches(request, request_i, self$request_matchers)) {
          return(i)
        }
      }
      return(NA_integer_)
    },

    # Returns response
    response_for = function(request) {
      i <- self$find_request(request)
      if (is.na(i)) {
        return(NULL)
      }

      self$used[[i]] <- TRUE
      self$interactions[[i]]$response
    },

    has_interaction = function(request) {
      i <- self$find_request(request, allow_playback = TRUE)
      !is.na(i)
    },

    has_used_interaction = function(request) {
      i <- self$find_request(request, allow_playback = TRUE)
      !is.na(i) && self$used[[i]]
    },

    remaining_unused_interaction_count = function() {
      sum(!self$used)
    }
  )
)
