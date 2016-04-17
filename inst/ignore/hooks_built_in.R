define_hook :before_record
# Adds a callback that will be called before the recorded HTTP interactions
# are serialized and written to disk.
#
# @example
#  VCR.configure do |c|
#    # Don't record transient 5xx errors
#    c.before_record do |interaction|
#      interaction.ignore! if interaction.response.status.code >= 500
#    end
#
#    # Modify the response body for cassettes tagged with :twilio
#    c.before_record(:twilio) do |interaction|
#      interaction.response.body.downcase!
#    end
#  end
#
# @param tag [(optional) Symbol] Used to apply this hook to only cassettes that match
#  the given tag.
# @yield the callback
# @yieldparam interaction [VCR::HTTPInteraction::HookAware] The interaction that will be
#  serialized and written to disk.
# @yieldparam cassette [(optional) VCR::Cassette] The current cassette.
# @see #before_playback
def before_record(tag = nil, &block)
  super(tag_filter_from(tag), &block)
end

define_hook :before_playback
# Adds a callback that will be called before a previously recorded
# HTTP interaction is loaded for playback.
#
# @example
#  VCR.configure do |c|
#    # Don't playback transient 5xx errors
#    c.before_playback do |interaction|
#      interaction.ignore! if interaction.response.status.code >= 500
#    end
#
#    # Change a response header for playback
#    c.before_playback(:twilio) do |interaction|
#      interaction.response.headers['X-Foo-Bar'] = 'Bazz'
#    end
#  end
#
# @param tag [(optional) Symbol] Used to apply this hook to only cassettes that match
#  the given tag.
# @yield the callback
# @yieldparam interaction [VCR::HTTPInteraction::HookAware] The interaction that is being
#  loaded.
# @yieldparam cassette [(optional) VCR::Cassette] The current cassette.
# @see #before_record
def before_playback(tag = nil, &block)
  super(tag_filter_from(tag), &block)
end

# Adds a callback that will be called with each HTTP request before it is made.
#
# @example
#  VCR.configure do |c|
#    c.before_http_request(:real?) do |request|
#      puts "Request: #{request.method} #{request.uri}"
#    end
#  end
#
# @param filters [optional splat of #to_proc] one or more filters to apply.
#   The objects provided will be converted to procs using `#to_proc`. If provided,
#   the callback will only be invoked if these procs all return `true`.
# @yield the callback
# @yieldparam request [VCR::Request::Typed] the request that is being made
# @see #after_http_request
# @see #around_http_request
define_hook :before_http_request

define_hook :after_http_request, :prepend
# Adds a callback that will be called with each HTTP request after it is complete.
#
# @example
#  VCR.configure do |c|
#    c.after_http_request(:ignored?) do |request, response|
#      puts "Request: #{request.method} #{request.uri}"
#      puts "Response: #{response.status.code}"
#    end
#  end
#
# @param filters [optional splat of #to_proc] one or more filters to apply.
#   The objects provided will be converted to procs using `#to_proc`. If provided,
#   the callback will only be invoked if these procs all return `true`.
# @yield the callback
# @yieldparam request [VCR::Request::Typed] the request that is being made
# @yieldparam response [VCR::Response] the response from the request
# @see #before_http_request
# @see #around_http_request
def after_http_request(*filters)
  super(*filters.map { |f| request_filter_from(f) })
end
