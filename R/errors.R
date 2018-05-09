#' UnhandledHTTPRequestError
#'
#' @export
#' @param request a request
#' @param cassette a cassette, the current cassette
#' @format NULL
#' @usage NULL
#' @details How this error class is used:
#' If `record="once"` we trigger this.
#'
#' Users can use vcr in the context of both use_cassette
#' and insert_cassette.
#'
#' For the former, all requests go through the call_block
#' But for the latter, requests go through webmockr
#'
#' Where is one place where we can put UnhandledHTTPRequestError
#' that will handle both use_cassette and insert_cassette?
#'
#' @section Error situations where this is invoked:
#'
#' - record=once AND there's a new request that doesn't match
#' the one in the cassette on disk
#'   - in webmockr: if no stub found and there are recorded
#'    interactions on the cassette, and record = once, then
#'    error with UnhandledHTTPRequestError
#'     - but if record != once, then allow it, unless record == none
#' - others?
#'
#' @examples
#' vcr_configure(dir = tempdir())
#' cassettes()
#' insert_cassette("turtle")
#' request <- Request$new("post", 'https://eu.httpbin.org/post?a=5',
#'   "", list(foo = "bar"))
#'
#' err <- UnhandledHTTPRequestError$new(request)
#' err$request_description()
#' err$current_matchers()
#' err$match_request_on_headers()
#' err$match_request_on_body()
#' err$formatted_headers()
#' cat(err$formatted_headers(), "\n")
#' cat(err$cassettes_description(), "\n")
#' cat(err$cassettes_list(), "\n")
#' err$formatted_suggestions()
#' cat(err$format_bullet_point('foo bar', 1), "\n")
#' err$suggestion_for("use_new_episodes")
#' err$suggestions()
#' err$no_cassette_suggestions()
#' err$record_mode_suggestion()
#' err$has_used_interaction_matching()
#' err$match_requests_on_suggestion()
#'
#' # err$construct_message()
#'
#' # cleanup
#' unlink(tempdir())
UnhandledHTTPRequestError <- R6::R6Class(
  "UnhandledHTTPRequestError",
  public = list(
    request = NULL,
    cassette = NULL,

    initialize = function(request, cassette) {
      assert(request, "Request")
      self$request <- request
      if (!missing(cassette)) {
        assert(cassette, "character")
        self$cassette <- cassette
      } else {
        self$cassette <- current_cassette()
      }
    },

    run = function() {
      any_errors <- FALSE
      if (!is.null(self$cassette) && !identical(self$cassette, list())) {
        if (self$cassette$record == "once") {
          any_errors <- TRUE
        }
      } else {
        if (identical(self$cassette, list())) any_errors <- TRUE
      }
      if (any_errors) self$construct_message()
      return(invisible())
    },

    construct_message = function() {
      mssg <- paste0(
        c("", "", paste0(rep("=", 80), collapse = ""),
          "An HTTP request has been made that vcr does not know how to handle:",
          self$request_description(),
          self$cassettes_description(),
          self$formatted_suggestions(),
          paste0(rep("=", 80), collapse = ""), "", ""),
        collapse = "\n")
      orig_warn_len <- getOption("warning.length")
      on.exit(options(warning.length = orig_warn_len))
      options(warning.length = 2000)
      stop(mssg, call. = FALSE)
    },

    request_description = function() {
      lines <- c()
      lines <- c(lines, paste(toupper(self$request$method), self$request$uri,
        sep = " "))
      if (self$match_request_on_headers()) {
        lines <- c(lines, sprintf("  Headers:\n%s", self$formatted_headers()))
      }
      if (self$match_request_on_body()) {
        lines <- c(lines, paste0("  Body: %s", self$request$body))
      }
      paste0(lines, collapse = "\n")
    },

    current_matchers = function() {
      if (length(cassettes_session()) > 0) {
        current_cassette()$match_requests_on
      } else {
        vcr_configuration()$match_requests_on
      }
    },

    match_request_on_headers = function() {
      "headers" %in% self$current_matchers()
    },

    match_request_on_body = function() {
      "body" %in% self$current_matchers()
    },

    formatted_headers = function() {
      tmp <- Map(function(a, b) {
        sprintf("    %s: %s", a, b)
      }, names(self$request$headers), self$request$headers)
      paste0(tmp, collapse = "\n")
    },

    cassettes_description = function() {
      if (length(cassettes_session()) > 0) {
        tmp <- self$cassettes_list()
        tmp2 <- paste0(c("\n",
         "Under the current configuration vcr can not find a suitable HTTP interaction",
         "to replay and is prevented from recording new requests. There are a few ways",
         "you can deal with this:\n"), collapse = "\n")
        c(tmp, tmp2)
      } else {
        paste0(c("There is currently no cassette in use. There are a few ways",
         "you can configure vcr to handle this request:\n"), collapse = "\n")
      }
    },

    cassettes_list = function() {
      lines <- c()
      xx <- if (length(cassettes_session()) == 1) {
        "vcr is currently using the following cassette:"
      } else {
        "vcr are currently using the following cassettes:"
      }
      lines <- c(lines, xx)
      # FIXME: should fix this to generalize to many cassettes, see ruby code
      zz <- c(
        paste0("  - ", self$cassette$file()),
        paste0("    - record_mode: ", self$cassette$record),
        paste0("    - match_requests_on: ",
        paste0(self$cassette$match_requests_on, collapse = ", "))
      )
      paste0(c(lines, zz), collapse = "\n")
    },

    formatted_suggestions = function() {
      formatted_points <- c()
      sugs <- self$suggestions()
      xx <- Map(function(bp, index) {
        fp <- c(formatted_points, self$format_bullet_point(bp$text, index))
        fn <- self$format_foot_note(bp$url, index)
        list(fp = fp, fn = fn)
      }, sugs, seq_along(sugs) - 1)
      paste0(c(vapply(xx, '[[', "", 1), vapply(xx, '[[', "", 2)),
             collapse = "\n", sep = "\n")
    },

    format_bullet_point = function(lines, index) {
      lines[1] <- paste0("  * ", lines[1])
      lines[length(lines)] <- paste(lines[length(lines)],
        sprintf("[%s].", index + 1))
      paste0(lines, collapse = "\n    ")
    },

    format_foot_note = function(url, index) {
      sprintf("[%s] %s", index + 1, url)
    },

    suggestion_for = function(key) {
      error_suggestions[[key]]
    },

    suggestions = function() {
      if (length(cassettes_session()) == 0) {
        return(self$no_cassette_suggestions())
      }

      tmp <- c("try_debug_logger", "use_new_episodes", "ignore_request")
      tmp <- c(tmp, self$record_mode_suggestion())
      if (self$has_used_interaction_matching()) tmp <- c(tmp, "allow_playback_repeats")
      tmp <- lapply(tmp, self$suggestion_for)
      compact(c(tmp, list(self$match_requests_on_suggestion())))
    },

    no_cassette_suggestions = function() {
      x <- c("try_debug_logger", "use_a_cassette",
        "allow_http_connections_when_no_cassette", "ignore_request")
      lapply(x, self$suggestion_for)
    },

    record_mode_suggestion = function() {
      record_modes <- unlist(lapply(cassettes_session(), function(z) z$record))

      if (all(record_modes == "none")) {
        "deal_with_none"
      } else if (all(record_modes == "once")) {
        "delete_cassette_for_once"
      } else {
        c()
      }
    },

    has_used_interaction_matching = function() {
      any(vapply(cassettes_session(), function(z) {
        z$http_interactions()
        z$http_interactions_$has_used_interaction_matching(self$request) %||% FALSE
      }, logical(1)))
    },

    match_requests_on_suggestion = function() {
      num_remaining_interactions <- sum(vapply(cassettes_session(), function(z) {
        z$http_interactions()
        z$http_interactions_$remaining_unused_interaction_count()
      }, numeric(1)))

      if (num_remaining_interactions == 0) return(NULL)

      interaction_description <- if (num_remaining_interactions == 1) {
        "1 HTTP interaction that has"
      } else {
        paste0(num_remaining_interactions, " HTTP interactions that have")
      }

      tmp <- self$suggestion_for("match_requests_on")
      description_lines <- tmp$text
      link <- tmp$url
      description_lines[1] <- sprintf(description_lines[1],
        interaction_description)
      list(text = paste0(description_lines, collapse = "\n"), url = link)
    }
  )
)
