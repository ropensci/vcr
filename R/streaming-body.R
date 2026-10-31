# A streaming response body that replays a recorded stream.
#
# httr2's `StreamingBody` wraps a connection, but `isIncomplete()` always
# returns `FALSE` for the connections we could build from recorded data, which
# would make `resp_stream_is_complete()` report `TRUE` before anything has been
# read. So instead of a connection, this holds a queue of raw chunks and
# implements the `StreamingBody` interface directly.
#
# Each call to `read()` returns at most one recorded chunk (or the first `n`
# bytes of it, leaving the rest for the next call), so the boundaries seen
# while recording are preserved on replay.
StreamingBodyReplay <- R6::R6Class(
  "StreamingBodyReplay",
  inherit = httr2::StreamingBody,
  public = list(
    initialize = function(chunks) {
      private$chunks <- chunks
      private$open <- TRUE
    },

    read = function(n) {
      if (!private$open || length(private$chunks) == 0) {
        return(raw())
      }

      chunk <- private$chunks[[1]]
      if (length(chunk) > n) {
        private$chunks[[1]] <- chunk[-seq_len(n)]
        chunk[seq_len(n)]
      } else {
        private$chunks <- private$chunks[-1]
        chunk
      }
    },

    is_open = function() {
      private$open
    },

    is_complete = function() {
      length(private$chunks) == 0
    },

    close = function() {
      private$open <- FALSE
      invisible()
    }
  ),
  private = list(
    chunks = list(),
    open = TRUE
  )
)

# Turn a complete body into the list of chunks it was recorded in. Falls back
# to a single chunk if the recorded sizes don't describe `body` (e.g. because
# the cassette was edited by hand, or the body was re-encoded when saved).
split_body_chunks <- function(body, sizes = NULL) {
  if (length(body) == 0) {
    return(list())
  }

  if (is.null(sizes) || sum(sizes) != length(body)) {
    sizes <- length(body)
  }
  sizes <- sizes[sizes > 0]

  ends <- cumsum(sizes)
  starts <- ends - sizes + 1
  unname(Map(function(start, end) body[start:end], starts, ends))
}

# Read a streaming response to the end, returning a list with one raw vector
# for each chunk of data that arrived. Intended for a response created with
# `blocking = FALSE`, so that chunks reflect what came off the network rather
# than the size of our read buffer. Closes the stream when done.
read_stream_chunks <- function(body, buffer = 64 * 1024) {
  defer(body$close())

  chunks <- list()
  while (!body$is_complete()) {
    chunk <- body$read(buffer)
    if (length(chunk) > 0) {
      chunks <- c(chunks, list(chunk))
    } else {
      Sys.sleep(0.01)
    }
  }

  # Pick up anything that arrived between the last read and completion
  repeat {
    chunk <- body$read(buffer)
    if (length(chunk) == 0) {
      break
    }
    chunks <- c(chunks, list(chunk))
  }

  chunks
}
