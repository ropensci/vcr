#' Null list, an empty HTTPInteractionList object
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
#' @param interactions (list) list of interaction class objects
#' @param request_matchers (character) vector of request matchers
#' @param allow_playback_repeats whether to allow playback repeats or not
#' @param parent_list A list for empty objects, see [NullList]
#' @param used_interactions (list) Interactions that have been used. That is,
#' interactions that are on disk in the current cassette, and a
#' request has been made that matches that interaction
#' @param request The request from an object of class `HttpInteraction`
#' @details
#' \strong{Methods}
#'   \describe{
#'     \item{\code{response_for(request)}}{
#'       Check if there's a matching interaction, returns a response
#'       object
#'     }
#'     \item{\code{has_interaction_matching(request)}}{
#'       Check if has a matching interaction. returns boolean
#'     }
#'     \item{\code{has_used_interaction_matching(request)}}{
#'       check if has used interactions matching a given request.
#'       returns boolean
#'     }
#'     \item{\code{remaining_unused_interaction_count()}}{
#'       Number of unused interactions. returns numeric
#'     }
#'     \item{\code{assert_no_unused_interactions()}}{
#'       Checks if there are no unused interactions left.
#'       returns boolean
#'     }
#'   }
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
#'  dir = tempdir(),
#'  record = "once"
#' )
#'
#' # make interactions
#' ## make the request
#' ### turn off mocking
#' crul::mock(FALSE)
#' url <- "https://eu.httpbin.org/post"
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
#' x$interactions
#' x$request_matchers
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

     initialize = function(interactions,
                           request_matchers,
                           allow_playback_repeats = FALSE,
                           parent_list = NullList$new(),
                           used_interactions = list()) {

       self$interactions <- interactions
       self$request_matchers <- request_matchers
       self$allow_playback_repeats <- allow_playback_repeats
       self$parent_list <- parent_list
       self$used_interactions <- used_interactions

       interaction_summaries <- vapply(interactions, function(x) {
         sprintf("%s => %s",
                 request_summary(Request$new()$from_hash(x$request)),
                 response_summary(VcrResponse$new()$from_hash(x$response)))
       }, "")
       vcr_log_info(sprintf(
         "Init. HTTPInteractionList w/ request matchers [%s] & %s interaction(s): { %s }",
         paste0(self$request_matchers, collapse = ", "),
         length(interactions),
         paste0(interaction_summaries, collapse = ', ')
       ), vcr_c$log_opts$date)
     },

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
         self$used_interactions <- unshift(self$used_interactions, list(interaction))
         vcr_log_info(sprintf("  Found matching interaction for %s at index %s: %s",
             request_summary(Request$new()$from_hash(request)),
             index,
             response_summary(VcrResponse$new()$from_hash(interaction$response))),
             vcr_c$log_opts$date)
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

     has_interaction_matching = function(request) {
       private$matching_interaction_bool(request) ||
         private$matching_used_interaction_for(request) ||
         self$parent_list$has_interaction_matching()
     },

     has_used_interaction_matching = function(request) {
       lapply(self$used_interactions, function(i) {
         private$interaction_matches_request(request, i)
       })
     },

     remaining_unused_interaction_count = function() {
       length(self$interactions)
     },

     # Checks if there are no unused interactions left.
     assert_no_unused_interactions = function() {
        if (!private$has_unused_interactions()) return(NULL)
        descriptions <- lapply(self$interactions, function(x) {
          sprintf("  - %s => %s",
            request_summary(x$request, self$request_matchers),
            response_summary(x$response))
        })
        vcr_log_info(descriptions, vcr_c$log_opts$date)
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
     matching_interaction_bool = function(request) {
       matchez <- vapply(self$interactions, function(w) {
         private$interaction_matches_request(request, w)
       }, logical(1))
       if (!any(matchez)) return(FALSE)
       any(matchez)
     },

     matching_interaction_index = function(request) {
       matchez <- vapply(self$interactions, function(w) {
         private$interaction_matches_request(request, w)
       }, logical(1))
       # if (!all(matchez)) return(numeric(0))
       if (!any(matchez)) return(numeric(0))
       which(matchez)
     },

     # return: interactions list
     matching_used_interaction_for = function(request) {
       if (!self$allow_playback_repeats) return(FALSE)
       tmp <- FALSE
       i <- 0
       while (!tmp) {
         i <- i + 1
         tmp <- private$interaction_matches_request(request,
          self$used_interactions[[i]])
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
       vcr_log_info(sprintf("  Checking if {%s} matches {%s} using matchers: [%s]",
           request_summary(req),
           request_summary(intreq),
           paste0(self$request_matchers, collapse = ", ")),
           vcr_c$log_opts$date)

       all(unlist(lapply(self$request_matchers, function(y) {
         matcher <- RequestMatcherRegistry$new()$registry[[y]]
         res <- matcher$matches(req, intreq)
         msg <- if (res) "matched" else "did not match"
         # cat(paste0("method: ", req$method), sep = "\n ")
         # cat(paste0("body: ", req$body), sep = "\n ")
         vcr_log_info(sprintf("    %s %s: current request [%s] vs [%s]",
             y, msg,
             request_summary(req, self$request_matchers), 
             request_summary(intreq, self$request_matchers)),
             vcr_c$log_opts$date)
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

# makes a copy - does not modify in place
# x: a list with objects of class HttpInteraction
# y: a list with an object of class HttpInteraction
unshift <- function(x, y) {
  stopifnot(inherits(x, "list"))
  stopifnot(inherits(y, "list"))
  # stopifnot(inherits(y[[1]], "HTTPInteraction"))
  c(y, x)
}
