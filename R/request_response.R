# The request of an {HTTPInteraction}.
#
# @attr [Symbol] method the HTTP method (i.e. :head, :options, :get, :post, :put, :patch or :delete)
# @attr [String] uri the request URI
# @attr [String, nil] body the request body
# @attr [Hash{String => Array<String>}] headers the request headers

#' Request class
#' @examples \dontrun{
#' (x <- Request$new())
#' }
Request <- R6::R6Class('Request',
   public = list(
     method = NULL,
     uri = NULL,
     body = NULL,
     headers = NULL,
     skip_port_stripping = FALSE,

     initialize = function(method, uri, body, headers) {
       if (!missing(method)) self$method <- tolower(method)
       if (!missing(uri)) {
         if (!skip_port_stripping) {
           self$uri <- self$without_standard_port(uri)
         } else {
           self$uri <- uri
         }
       }
       self$recorded_at <- Sys.time()
     },

     without_standard_port = function(uri) {
       if (is.null(uri))
       u <- self$parsed_uri(uri)
       if (paste0(u$scheme, u$port) %in% c('http', 'https/443')) {
         return(uri)
       }
       u$port = NULL
       # FIXME, don't love that I have to call build_url here
       return(httr::build_url(u))
     },

     parsed_uri = function(uri) {
       eval(parse(text = vcr_configuration()$uri_parser))(uri)
     },

     to_hash = function() {
       self$hash <- list(
         method  = self$method,
         uri     = self$uri,
         body    = serializable_body(self$body),
         headers = self$headers
       )
     },

     from_hash = function(hash) {
       method <- hash[['method']]
       list(
         hash[['uri']],
         body_from(hash[['body']]),
         hash[['headers']],
         self$skip_port_stripping
       )
     }
   )
)

serializable_body <- function(x) {
  if (is.null(x)) return(x)
  if (vcr_configuration()$preserve_exact_body_bytes_for) {
    base64enc::base64encode(charToRaw(x))
  } else {
    x
  }
}


#####
class Request < Struct.new(:method, :uri, :body, :headers)
  include Normalizers::Header
  include Normalizers::Body

  def initialize(*args)
    skip_port_stripping = false
    if args.last == :skip_port_stripping
      skip_port_stripping = true
      args.pop
    end

    super(*args)
    self.method = self.method.to_s.downcase.to_sym if self.method
    self.uri = without_standard_port(self.uri) unless skip_port_stripping
  end

  # Builds a serializable hash from the request data.
  #
  # @return [Hash] hash that represents this request and can be easily
  #  serialized.
  # @see Request.from_hash
  def to_hash
    {
      'method'  => method.to_s,
      'uri'     => uri,
      'body'    => serializable_body,
      'headers' => headers
    }.tap { |h| OrderedHashSerializer.apply_to(h, members) }
  end

  # Constructs a new instance from a hash.
  #
  # @param [Hash] hash the hash to use to construct the instance.
  # @return [Request] the request
  def self.from_hash(hash)
    method = hash['method']
    method &&= method.to_sym
    new method,
        hash['uri'],
        body_from(hash['body']),
        hash['headers'],
        :skip_port_stripping
  end

  # Parses the URI using the configured `uri_parser`.
  #
  # @return [#schema, #host, #port, #path, #query] A parsed URI object.
  def parsed_uri
    VCR.configuration.uri_parser.parse(uri)
  end

  @@object_method = Object.instance_method(:method)
  def method(*args)
    return super if args.empty?
    @@object_method.bind(self).call(*args)
  end

  # Decorates a {Request} with its current type.
  class Typed < DelegateClass(self)
    # @return [Symbol] One of `:ignored`, `:stubbed`, `:recordable` or `:unhandled`.
    attr_reader :type

    # @param [Request] request the request
    # @param [Symbol] type the type. Should be one of `:ignored`, `:stubbed`, `:recordable` or `:unhandled`.
    def initialize(request, type)
      @type = type
      super(request)
    end

    # @return [Boolean] whether or not this request is being ignored
    def ignored?
      type == :ignored
    end

    # @return [Boolean] whether or not this request is being stubbed by VCR
    # @see #externally_stubbed?
    # @see #stubbed?
    def stubbed_by_vcr?
      type == :stubbed_by_vcr
    end

    # @return [Boolean] whether or not this request is being stubbed by an
    #  external library (such as WebMock or FakeWeb).
    # @see #stubbed_by_vcr?
    # @see #stubbed?
    def externally_stubbed?
      type == :externally_stubbed
    end

    # @return [Boolean] whether or not this request will be recorded.
    def recordable?
      type == :recordable
    end

    # @return [Boolean] whether or not VCR knows how to handle this request.
    def unhandled?
      type == :unhandled
    end

    # @return [Boolean] whether or not this request will be made for real.
    # @note VCR allows `:ignored` and `:recordable` requests to be made for real.
    def real?
      ignored? || recordable?
    end

    # @return [Boolean] whether or not this request will be stubbed.
    #  It may be stubbed by an external library or by VCR.
    # @see #stubbed_by_vcr?
    # @see #externally_stubbed?
    def stubbed?
      stubbed_by_vcr? || externally_stubbed?
    end

    undef method
  end

  # Provides fiber-awareness for the {VCR::Configuration#around_http_request} hook.
  class FiberAware < DelegateClass(Typed)
    # Yields the fiber so the request can proceed.
    #
    # @return [VCR::Response] the response from the request
    def proceed
      Fiber.yield
    end

    # Builds a proc that allows the request to proceed when called.
    # This allows you to treat the request as a proc and pass it on
    # to a method that yields (at which point the request will proceed).
    #
    # @return [Proc] the proc
    def to_proc
      lambda { proceed }
    end

    undef method
  end

private

  def without_standard_port(uri)
    return uri if uri.nil?
    u = parsed_uri
    return uri unless [['http', 80], ['https', 443]].include?([u.scheme, u.port])
    u.port = nil
    u.to_s
  end
end

# The response of an {HTTPInteraction}.
#
# @attr [ResponseStatus] status the status of the response
# @attr [Hash{String => Array<String>}] headers the response headers
# @attr [String] body the response body
# @attr [nil, String] http_version the HTTP version
# @attr [Hash] adapter_metadata Additional metadata used by a specific VCR adapter.
class Response < Struct.new(:status, :headers, :body, :http_version, :adapter_metadata)
  include Normalizers::Header
  include Normalizers::Body

  def initialize(*args)
    super(*args)
    self.adapter_metadata ||= {}
  end

  # Builds a serializable hash from the response data.
  #
  # @return [Hash] hash that represents this response
  #  and can be easily serialized.
  # @see Response.from_hash
  def to_hash
    {
      'status'       => status.to_hash,
      'headers'      => headers,
      'body'         => serializable_body,
      'http_version' => http_version
    }.tap do |hash|
      hash['adapter_metadata'] = adapter_metadata unless adapter_metadata.empty?
      OrderedHashSerializer.apply_to(hash, members)
    end
  end

  # Constructs a new instance from a hash.
  #
  # @param [Hash] hash the hash to use to construct the instance.
  # @return [Response] the response
  def self.from_hash(hash)
    new ResponseStatus.from_hash(hash.fetch('status', {})),
        hash['headers'],
        body_from(hash['body']),
        hash['http_version'],
        hash['adapter_metadata']
  end

  # Updates the Content-Length response header so that it is
  # accurate for the response body.
  def update_content_length_header
    edit_header('Content-Length') { body ? body.bytesize.to_s : '0' }
  end

  # The type of encoding.
  #
  # @return [String] encoding type
  def content_encoding
    enc = get_header('Content-Encoding') and enc.first
  end

  # Checks if the type of encoding is one of "gzip" or "deflate".
  def compressed?
    %w[ gzip deflate ].include? content_encoding
  end

  # Decodes the compressed body and deletes evidence that it was ever compressed.
  #
  # @return self
  # @raise [VCR::Errors::UnknownContentEncodingError] if the content encoding
  #  is not a known encoding.
  def decompress
    self.class.decompress(body, content_encoding) { |new_body|
      self.body = new_body
      update_content_length_header
      delete_header('Content-Encoding')
    }
    return self
  end

  begin
    require 'zlib'
    require 'stringio'
    HAVE_ZLIB = true
  rescue LoadError
    HAVE_ZLIB = false
  end

  # Decode string compressed with gzip or deflate
  #
  # @raise [VCR::Errors::UnknownContentEncodingError] if the content encoding
  #  is not a known encoding.
  def self.decompress(body, type)
    unless HAVE_ZLIB
      warn "VCR: cannot decompress response; Zlib not available"
      return
    end

    case type
    when 'gzip'
      args = [StringIO.new(body)]
      args << { :encoding => 'ASCII-8BIT' } if ''.respond_to?(:encoding)
      yield Zlib::GzipReader.new(*args).read
    when 'deflate'
      yield Zlib::Inflate.inflate(body)
    when 'identity', NilClass
      return
    else
      raise Errors::UnknownContentEncodingError, "unknown content encoding: #{type}"
    end
  end
end
