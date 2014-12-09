#' Ejects the current cassette. The cassette will no longer be used.
#'
#' @export
#' @param (see VCR#eject_casssette)
#' @details In addition, any newly recorded HTTP interactions will be written to disk.
#' @note This is not intended to be called directly. Use \code{eject_cassette} instead.
eject <- function(options = list()){
  write_recorded_interactions_to_disk()

  if( should_assert_no_unused_interactions? && !options[:skip_no_unused_interactions_assertion]
  http_interactions.assert_no_unused_interactions!
}

write_recorded_interactions_to_disk <- function(){
  if(new_recorded_interactions() == "none")
  hash <- serializable_hash()
  if(hash["http_interactions"] == "none")

  @persister[storage_key] <- @serializer.serialize(hash)
}

should_assert_no_unused_interactions <- function(){
 !(@allow_unused_http_interactions || $!)
}

http_interactions <- function(){
  @http_interactions ||= HTTPInteractionList.new \
  should_stub_requests? ? previously_recorded_interactions : [],
  match_requests_on,
  @allow_playback_repeats,
  @parent_list,
  log_prefix
}

# #'
# #' # @private
# #' def record_http_interaction(interaction)
# #' log "Recorded HTTP interaction #{request_summary(interaction.request)} => #{response_summary(interaction.response)}"
# #' new_recorded_interactions << interaction
# #' end
# #'
# #' # @private
# #' def new_recorded_interactions
# #' @new_recorded_interactions ||= []
# #' end
# #'
# #' # @return [String] The file for this cassette.
# #' # @raise [NotImplementedError] if the configured cassette persister
# #' #  does not support resolving file paths.
# #' # @note VCR will take care of sanitizing the cassette name to make it a valid file name.
# #' def file
# #' unless @persister.respond_to?(:absolute_path_to_file)
# #' raise NotImplementedError, "The configured cassette persister does not support resolving file paths"
# #' end
# #' @persister.absolute_path_to_file(storage_key)
# #' end
# #'
# #' # @return [Boolean] Whether or not the cassette is recording.
# #' def recording?
# #' case record_mode
# #' when :none; false
# #' when :once; raw_cassette_bytes.to_s.empty?
# #' else true
# #' end
# #' end
# #'
# #' # @return [Hash] The hash that will be serialized when the cassette is written to disk.
# #' def serializable_hash
# #' {
# #'   "http_interactions" => interactions_to_record.map(&:to_hash),
# #'   "recorded_with"     => "VCR #{VCR.version}"
# #' }
# #' end
# #'
# #' # @return [Time, nil] The `recorded_at` time of the first HTTP interaction
# #' #                     or nil if the cassette has no prior HTTP interactions.
# #' #
# #' # @example
# #' #
# #' #   VCR.use_cassette("some cassette") do |cassette|
# #' #     Timecop.freeze(cassette.originally_recorded_at || Time.now) do
# #' #       # ...
# #' #     end
# #' #   end
# #' def originally_recorded_at
# #' @originally_recorded_at ||= previously_recorded_interactions.map(&:recorded_at).min
# #' end
# #'
# #' private
# #'
# #' def assert_valid_options!
# #'   invalid_options = @options.keys - [
# #'     :record, :erb, :match_requests_on, :re_record_interval, :tag, :tags,
# #'     :update_content_length_header, :allow_playback_repeats, :allow_unused_http_interactions,
# #'     :exclusive, :serialize_with, :preserve_exact_body_bytes, :decode_compressed_response,
# #'     :persist_with
# #'     ]
# #'
# #' if invalid_options.size > 0
# #' raise ArgumentError.new("You passed the following invalid options to VCR::Cassette.new: #{invalid_options.inspect}.")
# #' end
# #' end
# #'
# #' def extract_options
# #' [:erb, :match_requests_on, :re_record_interval,
# #'  :allow_playback_repeats, :allow_unused_http_interactions, :exclusive].each do |name|
# #'   instance_variable_set("@#{name}", @options[name])
# #' end
# #'
# #' assign_tags
# #'
# #' @record_mode = @options[:record]
# #' @serializer  = VCR.cassette_serializers[@options[:serialize_with]]
# #' @persister   = VCR.cassette_persisters[@options[:persist_with]]
# #' @record_mode = :all if should_re_record?
# #' @parent_list = @exclusive ? HTTPInteractionList::NullList : VCR.http_interactions
# #' end
# #'
# #' def assign_tags
# #' @tags = Array(@options.fetch(:tags) { @options[:tag] })
# #'
# #' [:update_content_length_header, :preserve_exact_body_bytes, :decode_compressed_response].each do |tag|
# #'   @tags << tag if @options[tag]
# #' end
# #' end
# #'
# #' def previously_recorded_interactions
# #' @previously_recorded_interactions ||= if !raw_cassette_bytes.to_s.empty?
# #' deserialized_hash['http_interactions'].map { |h| HTTPInteraction.from_hash(h) }.tap do |interactions|
# #'   invoke_hook(:before_playback, interactions)
# #'
# #' interactions.reject! do |i|
# #'   i.request.uri.is_a?(String) && VCR.request_ignorer.ignore?(i.request)
# #' end
# #' end
# #' else
# #'   []
# #' end
# #' end
# #'
# #' def storage_key
# #' @storage_key ||= [name, @serializer.file_extension].join('.')
# #' end
# #'
# #' def raise_error_unless_valid_record_mode
# #' unless VALID_RECORD_MODES.include?(record_mode)
# #' raise ArgumentError.new("#{record_mode} is not a valid cassette record mode.  Valid modes are: #{VALID_RECORD_MODES.inspect}")
# #' end
# #' end
# #'
# #' def should_re_record?
# #' return false unless @re_record_interval
# #' return false unless originally_recorded_at
# #'
# #' now = Time.now
# #'
# #' (originally_recorded_at + @re_record_interval < now).tap do |value|
# #'   info = "previously recorded at: '#{originally_recorded_at}'; now: '#{now}'; interval: #{@re_record_interval} seconds"
# #'
# #' if !value
# #' log "Not re-recording since the interval has not elapsed (#{info})."
# #' elsif InternetConnection.available?
# #' log "re-recording (#{info})."
# #' else
# #'   log "Not re-recording because no internet connection is available (#{info})."
# #' return false
# #' end
# #' end
# #' end
# #'
# #' def should_stub_requests?
# #' record_mode != :all
# #' end
# #'
# #' def should_remove_matching_existing_interactions?
# #' record_mode == :all
# #' end
# #'
# #' def should_assert_no_unused_interactions?
# #' !(@allow_unused_http_interactions || $!)
# #' end
# #'
# #' def raw_cassette_bytes
# #' @raw_cassette_bytes ||= VCR::Cassette::ERBRenderer.new(@persister[storage_key], erb, name).render
# #' end
# #'
# #' def merged_interactions
# #' old_interactions = previously_recorded_interactions
# #'
# #' if should_remove_matching_existing_interactions?
# #' new_interaction_list = HTTPInteractionList.new(new_recorded_interactions, match_requests_on)
# #' old_interactions = old_interactions.reject do |i|
# #'   new_interaction_list.response_for(i.request)
# #' end
# #' end
# #'
# #' old_interactions + new_recorded_interactions
# #' end
# #'
# #' def interactions_to_record
# #' # We deep-dup the interactions by roundtripping them to/from a hash.
# #' # This is necessary because `before_record` can mutate the interactions.
# #' merged_interactions.map { |i| HTTPInteraction.from_hash(i.to_hash) }.tap do |interactions|
# #'   invoke_hook(:before_record, interactions)
# #' end
# #' end
# #'
# #' def write_recorded_interactions_to_disk
# #' return if new_recorded_interactions.none?
# #' hash = serializable_hash
# #' return if hash["http_interactions"].none?
# #'
# #' @persister[storage_key] = @serializer.serialize(hash)
# #' end
# #'
# #' def invoke_hook(type, interactions)
# #' interactions.delete_if do |i|
# #'   i.hook_aware.tap do |hw|
# #'   VCR.configuration.invoke_hook(type, hw, self)
# #' end.ignored?
# #' end
# #' end
# #'
# #' def deserialized_hash
# #' @deserialized_hash ||= @serializer.deserialize(raw_cassette_bytes).tap do |hash|
# #'   unless hash.is_a?(Hash) && hash['http_interactions'].is_a?(Array)
# #' raise Errors::InvalidCassetteFormatError.new \
# #' "#{file} does not appear to be a valid VCR 2.0 cassette. " +
# #'   "VCR 1.x cassettes are not valid with VCR 2.0. When upgrading from " +
# #'   "VCR 1.x, it is recommended that you delete all your existing cassettes and " +
# #'   "re-record them, or use the provided vcr:migrate_cassettes rake task to migrate " +
# #'   "them. For more info, see the VCR upgrade guide."
# #' end
# #' end
# #' end
# #'
# #' def log_prefix
# #' @log_prefix ||= "[Cassette: '#{name}'] "
# #' end
# #'
# #' def request_summary(request)
# #' super(request, match_requests_on)
# #' end
# #' end
# #' end
