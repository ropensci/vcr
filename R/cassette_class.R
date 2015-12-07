#' Cassette handler
#'
#' @export
#' @examples \dontrun{
#' res <- Cassette$new("foobar")
#' res <- Cassette$new("foobar", record = "all")
#' res$file()
#' res$originally_recorded_at()
#' res$recording()
#' res$serializable_hash()
#' res$eject()
#' }
Cassette <- R6::R6Class("Cassette",
  public = list(
    name = NA,
    record = "none",
    manfile = NA,
    recorded_at = NA,
    initialize = function(name, record) {
      self$name <- name
      if (!missing(record)) self$record <- record
      make_dir("~/vcr/vcr_cassettes")
      self$manfile <- sprintf("%s/%s.yml", path.expand(cassette_path()), self$name)
      cat("\n", file = self$manfile)
      self$recorded_at <- file.info(self$file())$mtime
      message("Initialized with options: ", self$record)
    },
    eject = function() {
      stop("coming soon...")
      write_recorded_interactions_to_disk()
    },
    file = function() {
      self$manfile
    },
    recording = function() {
      if (self$record == "none") {
        FALSE
      } else {
        TRUE
      }
    },
    originally_recorded_at = function() {
      self$recorded_at
    },
    serializable_hash = function() {
      list(
        "http_interactions" = "fixme",
          # interactions_to_record.map(&:to_hash),
        "recorded_with"     = "fixme"
        # "VCR #{VCR.version}"
      )
    }
  )
)

make_dir <- function(x) {
  dir.create(x, showWarnings = FALSE, recursive = TRUE)
}

write_recorded_interactions_to_disk <- function(x) {
  if (length(any_new_recorded_interactions()) == 0) {
    message("none")
  }
#   hash = serializable_hash()
#   if (length(hash["http_interactions"])) {
#     message("none")
#   }

  message("fix me")
  # FIXME - need to make a persisters and serializer class/object
  # @persister[storage_key] = @serializer.serialize(hash)
}

# @return [Hash] The hash that will be serialized when the cassette is written to disk.
serializable_hash <- function() {
  "http_interactions" => interactions_to_record.map(&:to_hash),
  "recorded_with"     => "VCR #{VCR.version}"
}

interactions_to_record <- function(x) {
  "X"
}

interactions_to_record <- function() {
  # We deep-dup the interactions by roundtripping them to/from a hash.
  # This is necessary because `before_record` can mutate the interactions.
  merged_interactions.map { |i| HTTPInteraction.from_hash(i.to_hash) }.tap do |interactions|
    invoke_hook(:before_record, interactions)
  end
}

merged_interactions <- function(x) {
  old_interactions <- previously_recorded_interactions()

  if should_remove_matching_existing_interactions?
    new_interaction_list = HTTPInteractionList.new(new_recorded_interactions, match_requests_on)
    old_interactions = old_interactions.reject do |i|
      new_interaction_list.response_for(i.request)
    end
  end

  old_interactions + new_recorded_interactions
}

previously_recorded_interactions <- function() {
  @previously_recorded_interactions ||= if !raw_cassette_bytes.to_s.empty?
    deserialized_hash['http_interactions'].map { |h| HTTPInteraction.from_hash(h) }.tap do |interactions|
      invoke_hook(:before_playback, interactions)

      interactions.reject! do |i|
        i.request.uri.is_a?(String) && VCR.request_ignorer.ignore?(i.request)
      end
    end
  else
    []
  end
}

# raw_cassette_bytes()
raw_cassette_bytes <- function(name) {
  erbr_renderer(storage_key(name), erb, name)
  # @raw_cassette_bytes ||= VCR::Cassette::ERBRenderer.new(@persister[storage_key], erb, name).render
}

erbr_renderer <- function(key, erb, name) {
  return @raw_template if @raw_template.nil? || !use_erb?
  binding = binding_for_variables if erb_variables
  template.result(binding)
  # rescue NameError => e
  #   handle_name_error(e)
}

storage_key <- function(name) {
  paste0(name, serializer$file_extension, collapse = ".")
}

# deserialized_hash <- deserialized_hash(...)
deserialized_hash <- function() {
  tmp <- serializer$deserialize_string(raw_cassette_bytes()).tap
  if (is(tmp, "list") && length(tmp$http_interactions) > 0)) {
    tmp
  } else {
    stop(sprintf("%s does not appear to be a valid cassette", ), call. = FALSE)
  }
}

# library("httr")
# library("magrittr")
# Sys.setenv(VCR_LOG_PATH = "vcr.log")

# GET("http://google.com") %>% record_http_interaction
# ls(new_recorded_interactions)
# get(ls(new_recorded_interactions), envir = new_recorded_interactions)
#
# GET("http://api.crossref.org/members/78") %>% record_http_interaction
record_http_interaction <- function(x) {
  # FIXME - should be logged
  log_write(log_prepare(x))
  message(sprintf("Recorded HTTP interaction: %s => %s", x$request$url, response_summary(x)))
  assign(digest::digest(x), x, envir = new_recorded_interactions)
}

new_recorded_interactions <- new.env(parent = .GlobalEnv)

# any_new_recorded_interactions()
any_new_recorded_interactions <- function() {
  ls(new_recorded_interactions, envir = new_recorded_interactions)
}

log_prepare <- function(x) {
  sprintf("%s => %s", x$request$url, response_summary(x))
}

log_write <- function(x) {
  cat("\n", x, file = Sys.getenv("VCR_LOG_PATH"), append = TRUE)
}

# interactions_to_record <- function() {
# # We deep-dup the interactions by roundtripping them to/from a hash.
# # This is necessary because `before_record` can mutate the interactions.
#   merged_interactions.map { |i| HTTPInteraction.from_hash(i.to_hash) }.tap do |interactions|
#     invoke_hook(:before_record, interactions)
#   }
# }
