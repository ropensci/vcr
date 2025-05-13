UnhandledHTTPRequestError <- R6::R6Class(
  "UnhandledHTTPRequestError",
  public = list(
    # a `vcr_request` object
    request = NULL,
    cassette = NULL,

    initialize = function(request) {
      check_vcr_request(request)
      self$request <- request
      self$cassette <- current_cassette()
    },

    run = function() {
      # Don't trigger any logging while figuring out the error message
      local_vcr_configure_log(log = FALSE)

      any_errors <- FALSE
      if (!is.null(self$cassette)) {
        if (self$cassette$record %in% c("once", "none")) {
          any_errors <- TRUE
        }
      } else {
        any_errors <- TRUE
      }
      if (any_errors) self$construct_message()
      return(invisible())
    },

    construct_message = function() {
      # create formatted_suggestions for later use
      the$last_error <- list()
      the$last_error$request_description <- self$request_description()
      the$last_error$cassettes_description <- self$cassettes_description()
      the$last_error$formatted_suggestion <- self$formatted_suggestions()
      mssg <- paste0(
        c(
          paste0(rep("=", 80), collapse = ""),
          "An HTTP request has been made that vcr does not know how to handle:",
          self$request_description(),
          if (the$config$verbose_errors) self$cassettes_description() else
            self$cassettes_list(),
          if (the$config$verbose_errors) the$last_error$formatted_suggestion else
            self$get_help(),
          paste0(rep("=", 80), collapse = "")
        ),
        collapse = "\n"
      )
      abort(mssg, class = "vcr_unhandled", call = NULL)
    },

    request_description = function() {
      lines <- c()
      lines <- c(
        lines,
        paste(
          toupper(self$request$method),
          encode_sensitive(self$request$uri), # remove sensitive data
          sep = " "
        )
      )
      if (self$match_request_on_headers()) {
        lines <- c(
          lines,
          sprintf("  Headers:\n%s", encode_sensitive(self$formatted_headers()))
        )
      }
      if (self$match_request_on_body()) {
        lines <- c(lines, sprintf("  Body: %s", self$request$body))
      }
      paste0(lines, collapse = "\n")
    },

    current_matchers = function() {
      if (cassette_active()) {
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
      tmp <- Map(
        function(a, b) {
          sprintf("    %s: %s", a, b)
        },
        names(self$request$headers),
        self$request$headers
      )
      paste0(tmp, collapse = "\n")
    },

    cassettes_description = function() {
      if (cassette_active()) {
        tmp <- self$cassettes_list()
        tmp2 <- paste0(
          c(
            "\n",
            "Under the current configuration vcr can not find a suitable HTTP interaction",
            "to replay and is prevented from recording new requests. There are a few ways",
            "you can deal with this:\n"
          ),
          collapse = "\n"
        )
        c(tmp, tmp2)
      } else {
        "There is currently no cassette in use."
      }
    },

    cassettes_list = function() {
      if (cassette_active()) {
        lines <- c()
        xx <- if (length(the$cassettes) == 1) {
          "vcr is currently using the following cassette:"
        } else {
          "vcr is currently using the following cassettes:"
        }
        lines <- c(lines, xx)
        # FIXME: should fix this to generalize to many cassettes, see ruby code
        zz <- c(
          paste0("  - ", self$cassette$file()),
          paste0("    - record_mode: ", self$cassette$record),
          paste0(
            "    - match_requests_on: ",
            paste0(self$cassette$match_requests_on, collapse = ", ")
          )
        )
        paste0(c(lines, zz), collapse = "\n")
      } else {
        "There is currently no cassette in use.\n"
      }
    },

    get_help = function() {
      vm <- if (is_interactive()) "Run `vcr::vcr_last_error()`" else
        "Set `VCR_VERBOSE_ERRORS=TRUE`"
      c(
        paste0(vm, " for more verbose errors"),
        "If you're not sure what to do, open an issue https://github.com/ropensci/vcr/issues",
        "& see https://books.ropensci.org/http-testing"
      )
    },

    formatted_suggestions = function() {
      formatted_points <- c()
      sugs <- self$suggestions()
      xx <- Map(
        function(bp, index) {
          fp <- c(formatted_points, self$format_bullet_point(bp$text, index))
          fn <- self$format_foot_note(bp$url, index)
          list(fp = fp, fn = fn)
        },
        sugs,
        seq_along(sugs) - 1
      )
      paste0(
        c(vapply(xx, "[[", "", 1), "\n", vapply(xx, "[[", "", 2)),
        collapse = "",
        sep = "\n"
      )
    },

    format_bullet_point = function(lines, index) {
      lines[1] <- paste0("  * ", lines[1])
      lines[length(lines)] <- paste(
        lines[length(lines)],
        sprintf("[%s].", index + 1)
      )
      paste0(lines, collapse = "\n    ")
    },

    format_foot_note = function(url, index) {
      sprintf("[%s] %s", index + 1, url)
    },

    suggestion_for = function(key) {
      error_suggestions[[key]]
    },

    suggestions = function() {
      if (!cassette_active()) {
        return(self$no_cassette_suggestions())
      }

      tmp <- c("try_debug_logger", "use_new_episodes", "ignore_request")
      tmp <- c(tmp, self$record_mode_suggestion())
      if (self$has_used_interaction()) tmp <- c(tmp, "allow_playback_repeats")
      tmp <- lapply(tmp, self$suggestion_for)
      compact(c(tmp, list(self$match_requests_on_suggestion())))
    },

    no_cassette_suggestions = function() {
      x <- c(
        "try_debug_logger",
        "use_a_cassette",
        "ignore_request"
      )
      lapply(x, self$suggestion_for)
    },

    record_mode_suggestion = function() {
      record_modes <- unlist(lapply(the$cassettes, function(z) z$record))

      if (all(record_modes == "none")) {
        "deal_with_none"
      } else if (all(record_modes == "once")) {
        "delete_cassette_for_once"
      } else {
        c()
      }
    },

    has_used_interaction = function() {
      any(vapply(
        the$cassettes,
        function(z) {
          z$http_interactions$has_used_interaction(self$request) %||%
            FALSE
        },
        logical(1)
      ))
    },

    match_requests_on_suggestion = function() {
      num_remaining_interactions <- sum(vapply(
        the$cassettes,
        function(z) {
          z$http_interactions$n_replayable()
        },
        numeric(1)
      ))

      if (num_remaining_interactions == 0) return(NULL)

      interaction_description <- if (num_remaining_interactions == 1) {
        "1 HTTP interaction that has"
      } else {
        paste0(num_remaining_interactions, " HTTP interactions that have")
      }

      tmp <- self$suggestion_for("match_requests_on")
      description_lines <- tmp$text
      link <- tmp$url
      description_lines[1] <- sprintf(
        description_lines[1],
        interaction_description
      )
      list(text = paste0(description_lines, collapse = "\n    "), url = link)
    }
  )
)

#' Get full suggestion messages for the last vcr cassette failure
#'
#' @export
#' @rdname UnhandledHTTPRequestError
#' @examples \dontrun{
#' # Run after encountering a cassette error to get detailed information
#' vcr_last_error()
#' }
vcr_last_error <- function() {
  if (is.null(the$last_error) || length(the$last_error) == 0) {
    stop(
      "no error to report; either no cassette in use \n",
      "  or there's a problem with this package (i.e., open an issue)",
      call. = FALSE
    )
  }
  message(
    paste0(
      c(
        "",
        "",
        paste0(rep("=", 80), collapse = ""),
        the$last_error$request_description,
        the$last_error$cassettes_description,
        the$last_error$formatted_suggestion,
        paste0(rep("=", 80), collapse = ""),
        "",
        ""
      ),
      collapse = "\n"
    )
  )
}
