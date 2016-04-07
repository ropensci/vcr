#' HTTPInteractionList class
#' @export
#' @examples \dontrun{
#' (x <- HTTPInteractionList$new())
#' x$recorded_at
#' x$to_hash
#' x$from_hash
#' }
HTTPInteractionList <- R6::R6Class('HTTPInteractionList',
   # module NullList
   #   extend self
   #   def response_for(*a); nil; end
   #   def has_interaction_matching?(*a); false; end
   #   def has_used_interaction_matching?(*a); false; end
   #   def remaining_unused_interaction_count(*a); 0; end
   # end
   public = list(
     interactions           = NULL,
     request_matchers       = NULL,
     allow_playback_repeats = NULL,
     parent_list            = NULL,
     used_interactions      = NULL,
     log_prefix             = NULL,

     initialize = function(interactions, request_matchers, allow_playback_repeats = false, parent_list = NullList, log_prefix = '') {
       if (!missing(request)) self$request <- request
       if (!missing(response)) self$response <- response
       self$recorded_at <- Sys.time()

       interaction_summaries <- lapply(interactions, function(x) {
         sprintf("%s => %s", request_summary(x$request), response_summary(x$response))
       })
       cat("Initialized HTTPInteractionList with request matchers %s and %s interaction(s): { %s }",
           request_matchers, length(interactions), interaction_summaries.join(', '))
     },

     response_for = function(request) {
       if (index == matching_interaction_index_for(request)) {
          interaction = @interactions.delete_at(index)
          @used_interactions.unshift interaction
          log "Found matching interaction for #{request_summary(request)} at index #{index}: #{response_summary(interaction.response)}", 1
          interaction.response
        } elsif interaction = matching_used_interaction_for(request)
       interaction.response
       } else {
         @parent_list.response_for(request)
       }
     },

     matching_interaction_index_for = function() {

     },

     from_hash = function() {
       list(self$request$from_hash(hash['request']),
            self$response$from_hash(hash['response']),
            hash['recorded_at'])
     }
   )
)

class HTTPInteractionList
  def response_for(request)
    if index = matching_interaction_index_for(request)
      interaction = @interactions.delete_at(index)
      @used_interactions.unshift interaction
      log "Found matching interaction for #{request_summary(request)} at index #{index}: #{response_summary(interaction.response)}", 1
      interaction.response
    elsif interaction = matching_used_interaction_for(request)
      interaction.response
    else
      @parent_list.response_for(request)
    end
  end

  def has_interaction_matching?(request)
    !!matching_interaction_index_for(request) ||
    !!matching_used_interaction_for(request) ||
    @parent_list.has_interaction_matching?(request)
  end

  def has_used_interaction_matching?(request)
    @used_interactions.any? { |i| interaction_matches_request?(request, i) }
  end

  def remaining_unused_interaction_count
    @interactions.size
  end

  # Checks if there are no unused interactions left.
  #
  # @raise [VCR::Errors::UnusedHTTPInteractionError] if not all interactions were played back.
  def assert_no_unused_interactions!
    return unless has_unused_interactions?
    logger = Logger.new(nil)

    descriptions = @interactions.map do |i|
      "  - #{logger.request_summary(i.request, @request_matchers)} => #{logger.response_summary(i.response)}"
    end.join("\n")

    raise Errors::UnusedHTTPInteractionError, "There are unused HTTP interactions left in the cassette:\n#{descriptions}"
  end

private

  # @return [Boolean] Whether or not there are unused interactions left in the list.
  def has_unused_interactions?
    @interactions.size > 0
  end

  def request_summary(request)
    super(request, @request_matchers)
  end

  def matching_interaction_index_for(request)
    @interactions.index { |i| interaction_matches_request?(request, i) }
  end

  def matching_used_interaction_for(request)
    return nil unless @allow_playback_repeats
    @used_interactions.find { |i| interaction_matches_request?(request, i) }
  end

  def interaction_matches_request?(request, interaction)
    log "Checking if #{request_summary(request)} matches #{request_summary(interaction.request)} using #{@request_matchers.inspect}", 1
    @request_matchers.all? do |matcher_name|
      matcher = VCR.request_matchers[matcher_name]
      matcher.matches?(request, interaction.request).tap do |matched|
        matched = matched ? 'matched' : 'did not match'
        log "#{matcher_name} (#{matched}): current request #{request_summary(request)} vs #{request_summary(interaction.request)}", 2
      end
    end
  end

  def log_prefix
    @log_prefix
  end
end
