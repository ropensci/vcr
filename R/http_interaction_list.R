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
#'     \item{\code{response_for()}}{
#'       xxx.
#'     }
#'     \item{\code{has_interaction_matching()}}{
#'       xx.
#'     }
#'     \item{\code{has_used_interaction_matching()}}{
#'       xxx.
#'     }
#'     \item{\code{remaining_unused_interaction_count()}}{
#'       Number of unused interactions.
#'     }
#'     \item{\code{assert_no_unused_interactions()}}{
#'       xxx.
#'     }
#'   }
#' \strong{Private Methods}
#'  \describe{
#'     \item{\code{has_unused_interactions()}}{
#'       Are there areny unused interactions?
#'     }
#'     \item{\code{matching_interaction_index_for()}}{
#'       xxx.
#'     }
#'     \item{\code{matching_used_interaction_for()}}{
#'       xxxx.
#'     }
#'     \item{\code{interaction_matches_request()}}{
#'       Check if a request matches an interaction.
#'     }
#'     \item{\code{from_hash()}}{
#'       Get a hash back.
#'     }
#'     \item{\code{request_summary()}}{
#'       Get a request summary.
#'     }
#'     \item{\code{response_summary()}}{
#'       Get a response summary.
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
#' url <- "http://httpbin.org/post"
#' body <- list(foo = "bar")
#' res <- httr::POST(url, body = body)
#'
#' ## request
#' (request <- Request$new("POST", url, body, res$headers))
#' ## response
#' (response <- VcrResponse$new(
#'    c(status_code = res$status_code, http_status(res)),
#'    res$headers,
#'    content(res, "text"),
#'    res$all_headers[[1]]$version))
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
#' x$used_interactions
#' x$allow_playback_repeats
#' x$interactions
#' }
HTTPInteractionList <- R6::R6Class('HTTPInteractionList',
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
       TRUE
       # FIXME - finish this function, see bottom of page
       # if (index == matching_interaction_index_for(request)) {
       #   "adf"
       # } else {
       #   "adf"
       # }
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
        logger <- Logger$new(nil)

        descriptions <- lapply(self$interactions, function(x) {
          #"  - #{logger.request_summary(x.request, @request_matchers)} => #{logger.response_summary(x.response)}"
          sprintf("  - %s => %s",
                  logger$request_summary(x$request, self$request_matchers),
                  logger$response_summary(x$response))
        })

        stop("There are unused HTTP interactions left in the cassette:\n",
             descriptions, call. = FALSE)
     }
   ),

   private = list(
     has_unused_interactions = function() {
       length(self$interactions) > 0
     },

     matching_interaction_index_for = function(request) {
       vapply(self$interactions, function(x) {
         self$interaction_matches_request(request, x)
       }, logical(1))
     },

     matching_used_interaction_for = function(request) {
       if (!self$allow_playback_repeats) return(NULL)

       tmp <- FALSE
       i <- 0
       while (!tmp) {
         i <- i + 1
         tmp <- self$interaction_matches_request(request, self$used_interactions[[i]])
       }

       if (tmp) self$used_interactions[[i]] else NULL
     },

     interaction_matches_request = function(request, interaction) {
       # FIXME - log instead
       cat("Checking if %s matches %s using %s",
           request_summary(request),
           request_summary(interaction$request),
           paste0(self$request_matchers, collapse = ", "))

       lapply(self$request_matchers, function(y) {
         matcher <- RequestMatcherRegistry$new()$registry[[y]]
         res <- matcher$matches(request, interaction$request)
         msg <- if (res) "matched" else "did not match"
         # FIXME - log instead
         cat("%s %s: current request %s vs %s",
             y, msg, request_summary(request), request_summary(interaction.request))
         return(res)
       })
     },

     from_hash = function() {
       list(self$request$from_hash(hash['request']),
            self$response$from_hash(hash['response']),
            hash['recorded_at'])
     },

     request_summary = function(z) {
        paste(z$method, z$uri)
     },

     response_summary = function(z) {
       paste(
         z$status$status_code,
         sprintf("'%s ...'", substring(gsub("\n", "\\\\n", z$body), 1, 15)))
     }
   )
)

# response_for = function(request) {
#   if (index == matching_interaction_index_for(request)) {
#      interaction = @interactions.delete_at(index)
#      @used_interactions.unshift interaction
#      log "Found matching interaction for #{request_summary(request)} at index #{index}: #{response_summary(interaction.response)}", 1
#      interaction.response
#    } elsif interaction = matching_used_interaction_for(request)
#     interaction.response
#   } else {
#     parent_list.response_for(request)
#   }
# }
