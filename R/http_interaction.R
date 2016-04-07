#' HTTPInteraction class
#' @export
#' @examples \dontrun{
#' (x <- HTTPInteraction$new())
#' x$recorded_at
#' x$to_hash
#' x$from_hash
#' }
HTTPInteraction <- R6::R6Class('HTTPInteraction',
  public = list(
    request = NULL,
    response = NULL,
    recorded_at = NULL,

    initialize = function(request, response, recorded_at) {
      if (!missing(request)) self$request <- request
      if (!missing(response)) self$response <- response
      self$recorded_at <- Sys.time()
    },

    to_hash = function() {
      list(request = self$request$to_hash(),
           response = self$response$to_hash(),
           recorded_at = self$recorded_at)
    },

    from_hash = function() {
      list(self$request$from_hash(hash['request']),
           self$response$from_hash(hash['response']),
           hash['recorded_at'])
    }
  )
)
