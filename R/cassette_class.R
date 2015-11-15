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
      self$recorded_at <- file.info(res$file())$mtime
      message("Initialized with options: ", self$record)
    },
    eject = function() {
      write_recorded_interactions_to_disk()
      message("coming soon...")
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
  if (length(new_recorded_interactions) == 0) {
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

# library("httr")
# x <- GET("http://google.com")
# record_http_interaction(x)
# ls(new_recorded_interactions)
# get(ls(new_recorded_interactions), new_recorded_interactions)
record_http_interaction <- function(x) {
  # FIXME - should be logged
  message(sprintf("Recorded HTTP interaction: %s => %s", x$request$url, response_summary(x)))
  assign(digest::digest(x), x, envir = new_recorded_interactions)
}

new_recorded_interactions <- new.env(parent = .GlobalEnv)

# interactions_to_record <- function() {
# # We deep-dup the interactions by roundtripping them to/from a hash.
# # This is necessary because `before_record` can mutate the interactions.
#   merged_interactions.map { |i| HTTPInteraction.from_hash(i.to_hash) }.tap do |interactions|
#     invoke_hook(:before_record, interactions)
#   }
# }
