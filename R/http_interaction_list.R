# Null list, an empty HTTPInteractionList object
NullList <- R6::R6Class(
  'NullList',
  public = list(
    response_for = function() NULL,
    has_interaction_matching = function() FALSE,
    has_used_interaction_matching = function() FALSE,
    remaining_unused_interaction_count = function() 0
  )
)


#' @title HTTPInteractionList class
#' @description keeps track of all request-response pairs
#' @export
#' @param request A request object
#' @details
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{has_unused_interactions()}}{
#'       Are there any unused interactions? returns boolean
#'     }
#'     \item{\code{matching_interaction_index_for()}}{
#'       asdfadf
#'     }
#'     \item{\code{matching_used_interaction_for(request)}}{
#'       asdfadfs
#'     }
#'     \item{\code{interaction_matches_request(request, interaction)}}{
#'       Check if a request matches an interaction (logical)
#'     }
#'     \item{\code{request_summary(z)}}{
#'       Get a request summary (character)
#'     }
#'     \item{\code{response_summary(z)}}{
#'       Get a response summary (character)
#'     }
#'   }
HTTPInteractionList <- R6::R6Class(
  'HTTPInteractionList',
  public = list(
    #' @field interactions (list) list of interaction class objects
    interactions = NULL,
    #' @field request_matchers (character) vector of request matchers
    request_matchers = NULL,
    #' @field allow_playback_repeats whether to allow playback repeats
    allow_playback_repeats = FALSE,
    #' @field parent_list A list for empty objects, see `NullList`
    parent_list = NullList$new(),
    #' @field used_interactions (list) Interactions that have been used
    used_interactions = list(),

    #' @description Create a new `HTTPInteractionList` object
    #' @param interactions (list) list of interaction class objects
    #' @param request_matchers (character) vector of request matchers
    #' @param allow_playback_repeats whether to allow playback repeats or not
    #' @param parent_list A list for empty objects, see `NullList`
    #' @param used_interactions (list) Interactions that have been used. That is,
    #' interactions that are on disk in the current cassette, and a
    #' request has been made that matches that interaction
    #' @return A new `HTTPInteractionList` object
    initialize = function(
      interactions,
      request_matchers,
      allow_playback_repeats = FALSE,
      parent_list = NullList$new(),
      used_interactions = list()
    ) {
      self$interactions <- interactions
      self$request_matchers <- request_matchers
      self$allow_playback_repeats <- allow_playback_repeats
      self$parent_list <- parent_list
      self$used_interactions <- used_interactions
      interaction_summaries <- vapply(
        interactions,
        function(x) {
          sprintf(
            "%s => %s",
            request_summary(x$request),
            response_summary(x$response)
          )
        },
        ""
      )
      vcr_log_sprintf(
        "Init. HTTPInteractionList w/ request matchers [%s] & %s interaction(s): { %s }",
        paste0(self$request_matchers, collapse = ", "),
        length(interactions),
        paste0(interaction_summaries, collapse = ', ')
      )
    },

    #' @description Check if there's a matching interaction, returns a
    #' response object
    response_for = function(request) {
      index <- private$matching_interaction_index(request)
      if (length(index) > 0) {
        # index should be length 1 here it seems
        # FIXME: for now just get the first one
        # if (length(index > 1)) warning("more than 1 found, using first")
        index <- index[1]
        # delete the http interaction at <index>, and capture it into `interaction`
        interaction <- self$interactions[[index]]
        self$interactions <- delete_at(self$interactions, index)
        # put `interaction` at front of list with `unshift`
        self$used_interactions <- c(
          list(interaction),
          self$used_interactions
        )
        vcr_log_sprintf(
          "  Found matching interaction for %s at index %s: %s",
          request_summary(request),
          index,
          response_summary(interaction$response)
        )
        interaction$response
      } else {
        tmp <- private$matching_used_interaction_for(request)
        if (tmp) {
          tmp$response
        } else {
          self$parent_list$response_for()
        }
      }
    },

    #' @description Check if has a matching interaction
    #' @return logical
    has_interaction_matching = function(request) {
      private$matching_interaction_bool(request) ||
        private$matching_used_interaction_for(request) ||
        self$parent_list$has_interaction_matching()
    },

    #' @description check if has used interactions matching a given request
    #' @return logical
    has_used_interaction_matching = function(request) {
      lapply(self$used_interactions, function(i) {
        private$interaction_matches_request(request, i)
      })
    },

    #' @description Number of unused interactions
    #' @return integer
    remaining_unused_interaction_count = function() {
      length(self$interactions)
    }
  ),

  private = list(
    gather_match_checks = function(request) {
      out <- logical(0)
      iter <- 0
      while (!any(out) && iter < length(self$interactions)) {
        iter <- iter + 1
        bool <- private$interaction_matches_request(
          request,
          self$interactions[[iter]]
        )
        out <- c(out, bool)
      }
      return(out)
    },

    # return: logical
    matching_interaction_bool = function(request) {
      any(private$gather_match_checks(request))
    },

    # return: integer
    matching_interaction_index = function(request) {
      which(private$gather_match_checks(request))
    },

    # return: interactions list or `FALSE`
    matching_used_interaction_for = function(request) {
      if (!self$allow_playback_repeats) return(FALSE)
      if (length(self$used_interactions) == 0) return(FALSE)
      tmp <- FALSE
      i <- 0
      while (!tmp) {
        i <- i + 1
        tmp <- private$interaction_matches_request(
          request,
          self$used_interactions[[i]]
        )
      }
      if (tmp) self$used_interactions[[i]] else FALSE
    },

    # return: interactions list
    interaction_matches_request = function(req, interaction) {
      bod <- interaction$request$body
      if (length(names(bod)) > 0) {
        if ("string" %in% names(bod)) bod <- bod$string
      }
      intreq <- Request$new(
        interaction$request$method,
        interaction$request$uri,
        bod,
        interaction$request$headers
      )
      vcr_log_sprintf(
        "  Checking if {%s} matches {%s} using matchers: [%s]",
        request_summary(req),
        request_summary(intreq),
        paste0(self$request_matchers, collapse = ", ")
      )

      all(unlist(lapply(self$request_matchers, function(y) {
        matcher <- RequestMatcherRegistry$new()$registry[[y]]
        res <- matcher$matches(req, intreq)
        msg <- if (res) "matched" else "did not match"
        # cat(paste0("method: ", req$method), sep = "\n ")
        # cat(paste0("body: ", req$body), sep = "\n ")
        vcr_log_sprintf(
          "    %s %s: current request [%s] vs [%s]",
          y,
          msg,
          request_summary(req, self$request_matchers),
          request_summary(intreq, self$request_matchers)
        )
        return(res)
      })))
    },

    # return: character
    request_summary = function(z) {
      paste(z$method, z$uri)
    },

    # return: character
    response_summary = function(z) {
      paste(
        z$status$status_code,
        sprintf("['%s ...'", substring(gsub("\n", " ", z$body), 1, 50)),
        "]"
      )
    }
  )
)

# makes a copy - does not modify in place
# x: must be a list
# y: must be numeric; ignores values out of range
delete_at <- function(x, y) {
  stopifnot(is.list(x))
  stopifnot(is.numeric(y))
  x[-y]
}
