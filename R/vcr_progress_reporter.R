#' @export
VcrProgressReporter <- R6::R6Class("VcrProgressReporter",
  inherit = ProgressReporter,
  public = list(
    vcr_cassettes = list(),

    start_context = function(context) {
      self$vcr_cassettes <- list()
      
      self$ctxt_name <- context
      self$ctxt_issues <- Stack$new()

      self$ctxt_n <- 0L
      self$ctxt_n_ok <- 0L
      self$ctxt_n_fail <- 0L
      self$ctxt_n_warn <- 0L
      self$ctxt_n_skip <- 0L

      self$ctxt_start_time <- proc.time()
      self$show_status()
    },

    end_context = function(context) {
      time <- proc.time() - self$ctxt_start_time

      self$last_update <- NULL
      self$show_status(complete = TRUE)

      if (time[[3]] > self$min_time) {
        self$cat_line(sprintf(" [%.1f s]", time[[3]]), col = "cyan")
      } else {
        self$cat_line()
      }

      out <- vapply(self$vcr_cassettes, function(w) {
        sprintf("     %s --- %s", w$name, w$recorded_at)
      }, "")

      # self$cat_line("vcr: cassette --- last recorded")
      self$cat_line(unique(out), col = "lightblue")

      if (self$ctxt_issues$size() > 0) {
        self$rule()

        issues <- self$ctxt_issues$as_list()
        summary <- vapply(issues, issue_summary, FUN.VALUE = character(1))
        self$cat_tight(paste(summary, collapse = "\n\n"))

        self$cat_line()
        self$rule()
      }
    },

    add_result = function(context, test, result) {
      self$vcr_cassettes <- c(self$vcr_cassettes, 
        cassette_runner$running)
      self$ctxt_n <- self$ctxt_n + 1L

      if (expectation_broken(result)) {
        self$n_fail <- self$n_fail + 1
        self$ctxt_n_fail <- self$ctxt_n_fail + 1
        self$ctxt_issues$push(result)
      } else if (expectation_skip(result)) {
        self$n_skip <- self$n_skip + 1
        self$ctxt_n_skip <- self$ctxt_n_skip + 1
        self$ctxt_issues$push(result)
      } else if (expectation_warning(result)) {
        self$n_warn <- self$n_warn + 1
        self$ctxt_n_warn <- self$ctxt_n_warn + 1
        self$ctxt_issues$push(result)
      } else {
        self$n_ok <- self$n_ok + 1
        self$ctxt_n_ok <- self$ctxt_n_ok + 1
      }

      self$show_status()
    }

  )
)


spinner <- function(i) {
  frames <- cli::get_spinner()$frames
  frames[((i - 1) %% length(frames)) + 1]
}

issue_summary <- function(x) {
  type <- expectation_type(x)

  if (is.null(x$srcref)) {
    loc <- "???"
  } else {
    filename <- attr(x$srcref, "srcfile")$filename
    loc <- paste0(basename(filename), ":", x$srcref[1])
  }

  header <- paste0(loc, ": ", colourise(type, type), ": ", x$test)

  paste0(
    crayon::bold(header), "\n",
    format(x)
  )
}

expectation_type <- function(exp) {
  stopifnot(is.expectation(exp))
  gsub("^expectation_", "", class(exp)[[1]])
}

expectation_success <- function(exp) {
  expectation_type(exp) == "success"
}

expectation_failure <- function(exp) {
  expectation_type(exp) == "failure"
}

expectation_error <- function(exp) {
  expectation_type(exp) == "error"
}

expectation_skip <- function(exp) {
  expectation_type(exp) == "skip"
}

expectation_warning <- function(exp) {
  expectation_type(exp) == "warning"
}

expectation_broken <- function(exp) {
  expectation_failure(exp) || expectation_error(exp)
}
expectation_ok <- function(exp) {
  expectation_type(exp) %in% c("success", "warning")
}



as.expectation <- function(x, ...) UseMethod("as.expectation", x)

#' @export
as.expectation.default <- function(x, ..., srcref = NULL) {
  stop(
    "Don't know how to convert '", paste(class(x), collapse = "', '"),
    "' to expectation.", call. = FALSE
  )
}

#' @export
as.expectation.expectation <- function(x, ..., srcref = NULL) {
  if (is.null(x$srcref)) {
    x$srcref <- srcref
  }
  x
}

#' @export
as.expectation.logical <- function(x, message, ..., srcref = NULL, info = NULL) {
  type <- if (x) "success" else "failure"
  message <- if (x) "success" else add_info(message, info)
  expectation(type, message, srcref = srcref)
}

add_info <- function(message, info = NULL) {
  paste(c(message, info), collapse = "\n")
}

#' @export
as.expectation.error <- function(x, ..., srcref = NULL) {
  error <- x$message

  msg <- gsub("Error.*?: ", "", as.character(error))

  # Need to remove trailing newline from error message to be consistent
  # with other messages
  msg <- gsub("\n$", "", msg)

  expectation("error", msg, srcref)
}

#' @export
as.expectation.warning <- function(x, ..., srcref = NULL) {
  msg <- x$message

  # msg <- gsub("Error.*?: ", "", as.character(error))
  # msg <- gsub("\n$", "", msg)

  expectation("warning", msg, srcref)
}

#' @export
as.expectation.skip <- function(x, ..., srcref = NULL) {
  error <- x$message
  msg <- gsub("Error.*?: ", "", as.character(error))

  expectation("skip", msg, srcref)
}

#' @export
print.expectation <- function(x, ...) cat(format(x), "\n")

#' @export
format.expectation_success <- function(x, ...) {
  "As expected"
}

#' @export
format.expectation_error <- function(x, ...) {
  paste(c(x$message, create_traceback(x$call)), collapse = "\n")
}

#' @export
format.expectation <- function(x, ...) {
  x$message
}

single_letter_summary <- function(x) {
  switch(expectation_type(x),
    skip    = colourise("S", "skip"),
    success = colourise(".", "success"),
    error   = colourise("E", "error"),
    failure = colourise("F", "failure"),
    warning = colourise("W", "warning"),
    "?"
  )
}

colourise <- function(text, as = c("success", "skip", "warning", "failure", "error")) {
  colour_config <- getOption("testthat.use_colours", TRUE)
  if (!isTRUE(colour_config)) {
    return(text)
  }

  crayon::style(text, testthat_style(as))
}

testthat_style <- function(type = c("success", "skip", "warning", "failure", "error")) {
  type <- match.arg(type)

  c(
    success = "green",
    skip = "blue",
    warning = "magenta",
    failure = "red",
    error = "red"
  )[[type]]
}

create_traceback <- function(callstack) {
  if (length(callstack) == 0) return()
  max_lines <- getOption("deparse.max.lines", Inf)

  # Convert to text
  calls <- lapply(callstack, deparse, width.cutoff = cli::console_width())
  if (is.finite(max_lines)) {
    calls <- lapply(calls, function(x) x[seq_len(min(length(x), max_lines))])
  }
  calls <- vapply(calls, paste0, collapse = "\n", FUN.VALUE = character(1))

  # Extract srcrefs
  srcrefs <- lapply(callstack, attr, "srcref")
  has_ref <- vapply(srcrefs, function(x) inherits(x, "srcref"), logical(1))
  files <- vapply(
    srcrefs[has_ref], function(x) attr(x, "srcfile")$filename,
    FUN.VALUE = character(1)
  )
  lines <- vapply(
    srcrefs[has_ref], function(x) as.vector(x)[1],
    FUN.VALUE = integer(1)
  )

  calls[has_ref] <- paste0(calls[has_ref], " at ", files, ":", lines)

  # Number and indent
  calls <- paste0(seq_along(calls), ": ", calls)
  calls <- gsub("\n", "\n   ", calls)

  first_last(calls)
}

first_last <- function(x, max = 10, filler = "...") {
  if (length(x) <= 2 * max + 1) {
    x
  } else {
    c(
      x[seq_len(max)],
      filler,
      x[seq.int(to = length(x), length.out = max)]
    )
  }
}

# Source: https://github.com/rstudio/shiny/blob/master/R/stack.R
# License: GPL-3
# Relicensed a MIT with permission.

# A Stack object backed by a list. The backing list will grow or shrink as
# the stack changes in size.
Stack <- R6Class(
  "Stack",
  class = FALSE,
  public = list(
    initialize = function(init = 20L) {
      # init is the initial size of the list. It is also used as the minimum
      # size of the list as it shrinks.
      private$stack <- vector("list", init)
      private$init <- init
      private$count <- 0L
    },

    push = function(..., .list = NULL) {
      args <- c(list(...), .list)
      new_size <- private$count + length(args)

      # Grow if needed; double in size
      while (new_size > length(private$stack)) {
        private$stack[length(private$stack) * 2L] <- list(NULL)
      }
      private$stack[private$count + seq_along(args)] <- args
      private$count <- new_size

      invisible(self)
    },

    size = function() {
      private$count
    },

    # Return the entire stack as a list, where the first item in the list is the
    # oldest item in the stack, and the last item is the most recently added.
    as_list = function() {
      private$stack[seq_len(private$count)]
    }
  ),

  private = list(
    stack = NULL,   # A list that holds the items
    count = 0L,     # Current number of items in the stack
    init = 20L      # Initial and minimum size of the stack
  )
)

