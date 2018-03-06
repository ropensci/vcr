NullList <- R6::R6Class(
  'NullList',
  public = list(
    response_for = function() NULL,
    has_interaction_matching = function() FALSE,
    has_used_interaction_matching = function() FALSE,
    remaining_unused_interaction_count = function() 0
  )
)


#' HTTPInteractionList class
#'
#' @export
#' @param interactions list of interaction class
#' @param request_matchers xxx
#' @param allow_playback_repeats xxx
#' @param parent_list xxx
#' @param used_interactions xxx
#' @param log_prefix xxx
#' @param request xxx
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{response_for(request)}}{
#'       Check if there's a matching interaction
#'     }
#'     \item{\code{has_interaction_matching(request)}}{
#'       Check if has a matching interaction
#'     }
#'     \item{\code{has_used_interaction_matching(request)}}{
#'       check if has used interactions matching a given request
#'     }
#'     \item{\code{remaining_unused_interaction_count()}}{
#'       Number of unused interactions (numeric)
#'     }
#'     \item{\code{assert_no_unused_interactions()}}{
#'       Checks if there are no unused interactions left.
#'     }
#'   }
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{has_unused_interactions()}}{
#'       Are there areny unused interactions?
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
#'     \item{\code{from_hash()}}{
#'       Get a hash back.
#'     }
#'     \item{\code{request_summary(z)}}{
#'       Get a request summary (character)
#'     }
#'     \item{\code{response_summary(z)}}{
#'       Get a response summary (character)
#'     }
#'   }
#' @format NULL
#' @usage NULL
#' @examples \dontrun{
#' vcr_configure(
#'  dir = "fixtures/vcr_cassettes",
#'  record = "once"
#' )
#'
#' # make interactions
#' ## make the request
#' ### turn off mocking
#' crul::mock(FALSE)
#' url <- "https://httpbin.org/post"
#' cli <- crul::HttpClient$new(url = url)
#' res <- cli$post(body = list(a = 5))
#'
#' ## request
#' (request <- Request$new("POST", url, body, res$headers))
#' ## response
#' (response <- VcrResponse$new(
#'    res$status_http(),
#'    res$response_headers,
#'    res$parse("UTF-8"),
#'    res$response_headers$status))
#' ## make an interaction
#' (inter <- HTTPInteraction$new(request = request, response = response))
#'
#' # make an interactionlist
#' (x <- HTTPInteractionList$new(
#'    interactions = list(inter),
#'    request_matchers = vcr_configuration()$match_requests_on
#' ))
#' x$request_matchers
#' x$log_prefix
#' x$parent_list
#' x$parent_list$response_for()
#' x$parent_list$has_interaction_matching()
#' x$parent_list$has_used_interaction_matching()
#' x$parent_list$remaining_unused_interaction_count()
#' x$used_interactions
#' x$allow_playback_repeats
#' x$interactions
#' x$response_for(request)
#' }
HTTPInteractionList <- R6::R6Class(
  'HTTPInteractionList',
   public = list(
     interactions           = NULL,
     request_matchers       = NULL,
     allow_playback_repeats = FALSE,
     parent_list            = NullList$new(),
     used_interactions      = list(),
     log_prefix             = NULL,

     initialize = function(interactions,
                           request_matchers,
                           allow_playback_repeats = FALSE,
                           parent_list = NullList$new(),
                           used_interactions = list(),
                           log_prefix = '') {

       self$interactions <- interactions
       self$request_matchers <- request_matchers
       self$allow_playback_repeats <- allow_playback_repeats
       self$parent_list <- parent_list
       self$used_interactions <- used_interactions
       self$log_prefix <- log_prefix

       interaction_summaries <- vapply(interactions, function(x) {
         sprintf("[%s] => [%s]",
                 private$request_summary(x$request),
                 private$response_summary(x$response))
       }, "")
       message(sprintf(
         "Initialized HTTPInteractionList with request matchers [%s] and %s interaction(s): { %s }",
         paste0(self$request_matchers, collapse = ", "),
         length(interactions),
         paste0(interaction_summaries, collapse = ', ')
       ))
     },

     response_for = function(request) {
       index <- private$matching_interaction_index_for(request)
       if (length(index) > 0) {
         # delete the http interaction at <index>, and capture it into `interaction`
         interaction <- self$interactions[[index]]
         self$interactions <- delete_at(self$interactions, index)
         # put `interaction` at front of list with `unshift`
         self$used_interactions <- unshift(self$used_interactions, list(interaction))
         cat(sprintf("Found matching interaction for %s at index %s: %s",
             private$request_summary(request),
             index,
             private$response_summary(interaction$response)), sep = "\n")
         interaction$response
       } else if (interaction == self$matching_used_interaction_for(request)) {
         interaction$response
       } else {
         self$parent_list$response_for(request)
       }
     },

     has_interaction_matching = function(request) {
       self$matching_interaction_index_for(request) ||
         self$matching_used_interaction_for(request) ||
         self$parent_list$has_interaction_matching()
     },

     has_used_interaction_matching = function(request) {
       lapply(self$used_interactions, function(i) {
         self$interaction_matches_request(request, i)
       })
     },

     remaining_unused_interaction_count = function() {
       length(self$interactions)
     },

     # Checks if there are no unused interactions left.
     # @raise [VCR::Errors::UnusedHTTPInteractionError] if not all interactions were played back.
     assert_no_unused_interactions = function() {
        if (!has_unused_interactions()) return()
        # FIXME: replace cat() with logging
        # logger <- Logger$new(nil)

        descriptions <- lapply(self$interactions, function(x) {
          #"  - #{logger.request_summary(x.request, @request_matchers)} => #{logger.response_summary(x.response)}"
          # sprintf("  - %s => %s",
          #         logger$request_summary(x$request, self$request_matchers),
          #         logger$response_summary(x$response))
          cat(sprintf("  - %s => %s",
                  private$request_summary(x$request, self$request_matchers),
                  private$response_summary(x$response)))
        })

        stop("There are unused HTTP interactions left in the cassette:\n",
             descriptions, call. = FALSE)
     }
   ),

   private = list(
     # return: logical
     has_unused_interactions = function() {
       length(self$interactions) > 0
     },

     # return: integer
     matching_interaction_index_for = function(request) {
       which(vapply(self$interactions, function(w) {
         private$interaction_matches_request(request, w)
       }, logical(1)))
     },

     # return: interactions list
     matching_used_interaction_for = function(request) {
       if (!self$allow_playback_repeats) return(NULL)

       tmp <- FALSE
       i <- 0
       while (!tmp) {
         i <- i + 1
         tmp <- private$interaction_matches_request(request, self$used_interactions[[i]])
       }

       if (tmp) self$used_interactions[[i]] else NULL
     },

     # return: interactions list
     interaction_matches_request = function(request, interaction) {
       # FIXME - log instead
       cat(sprintf("Checking if %s matches %s using %s",
           private$request_summary(request),
           private$request_summary(interaction$request),
           paste0(self$request_matchers, collapse = ", ")),
           sep = "\n")

       all(unlist(lapply(self$request_matchers, function(y) {
         matcher <- RequestMatcherRegistry$new()$registry[[y]]
         res <- matcher$matches(request, interaction$request)
         msg <- if (res) "matched" else "did not match"
         # FIXME - log instead
         cat(sprintf("%s %s: current request %s vs %s",
             y, msg,
             private$request_summary(request),
             private$request_summary(interaction$request)), sep = "\n")
         return(res)
       })))
     },

     # FIXME: not sure why this is here, should only belong in HTTPInteraction I think
     # from_hash = function() {
     #   list(self$request$from_hash(hash['request']),
     #        self$response$from_hash(hash['response']),
     #        hash['recorded_at'])
     # },

     # return: character
     request_summary = function(z) {
        paste(z$method, z$uri)
     },

     # return: character
     response_summary = function(z) {
       paste(
         z$status$status_code,
         sprintf("['%s ...'", substring(gsub("\n", "\\\\n", z$body), 1, 50)),
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
# makes a copy - does not modify in place
# x: a list with objects of class HttpInteraction
# y: a list with an object of class HttpInteraction
unshift <- function(x, y) {
  stopifnot(inherits(x, "list"))
  stopifnot(inherits(y, "list"))
  stopifnot(inherits(y[[1]], "HTTPInteraction"))
  c(y, x)
}
