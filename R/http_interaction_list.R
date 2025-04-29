HTTPInteractionList <- R6::R6Class(
  'HTTPInteractionList',
  public = list(
    interactions = NULL,
    used = logical(),

    request_matchers = NULL,
    allow_playback_repeats = FALSE,

    initialize = function(
      interactions = list(),
      request_matchers = c("method", "uri"),
      allow_playback_repeats = FALSE,
      replayable = TRUE
    ) {
      self$interactions <- interactions
      self$used <- rep(!replayable, length(interactions))

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

    add = function(request, response, overwrite = TRUE) {
      n <- length(self$interactions)
      if (overwrite) {
        idx <- self$find_request(request, allow_playback = TRUE)
        if (is.na(idx)) {
          idx <- n + 1
        }
      } else {
        idx <- n + 1
      }

      interaction <- list(request = request, response = response)
      self$interactions[[idx]] <- interaction
      self$used[[idx]] <- TRUE # don't allow playback for new interactions

      interaction
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
