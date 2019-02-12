expectation_error <- function(exp) {
  exp <- gsub("^expectation_", "", class(exp)[[1]])
  exp == "error"
}

farts <- list()

vcr_reporter <- R6::R6Class("vcr_reporter",
  inherit = testthat::ProgressReporter,
  # inherit = testthat::Reporter,
  public = list(
    # initialize = function(
    #   file = getOption("testthat.output_file", stdout()),
    #   min_time = 0.1, ...) {

    #   super$initialize(min_time = min_time, file = file, ...)
    # },

    # start_test = function(context, test) {
    #   super$start_test(context, test)
    # },

    # end_test = function(context, test) {
    #   super$end_test(context, test)
    # },

    # start_reporter = function(context) {
    #   super$start_reporter(context)
    # },

    end_reporter = function() {
      super$end_reporter()

      cli::cat_line("vcr: 3 of 6 using cached HTTP responses")
    },

    # add_result = function(context, test, result) {
    #   if (expectation_error(result)) {
    #     self$failure <- result
    #   }
    # }

    add_result = function(context, test, result) {
      ref <- result$srcref
      if (is.null(ref)) {
        location <- "?#?:?"
      } else {
        location <- paste0(basename(attr(ref, "srcfile")$filename), "#", ref[1], ":1")
      }

      farts <<- result
      # cat("\n", attributes(ref), sep = "\n")

      status <- testthat:::expectation_type(result)
      self$cat_line("  ", location, " [", status, "]")
    },

    # start_context = function(context) {
    #   super$start_context(context)
    # },

    # farts = function(context) {
    #   proc.time() - super$ctxt_start_time
    # },

    end_context = function(context) {
      time <- proc.time() - self$ctxt_start_time

      super$last_update <- NULL
      self$show_status(complete = TRUE)

      if (time[[3]] > self$min_time) {
        self$cat_line(sprintf(" [%.1f s] [1 of 2]", time[[3]]), col = "cyan")
        # self$cat_line(" vcr!", col = "green")
      } else {
        self$cat_line()
      }

      if (self$ctxt_issues$size() > 0) {
        self$rule()

        issues <- self$ctxt_issues$as_list()
        summary <- vapply(issues, testthat:::issue_summary,
          FUN.VALUE = character(1))
        self$cat_tight(paste(summary, collapse = "\n\n"))

        self$cat_line()
        self$rule()
      }
    }

    # detect_vcr_usage = function(x) {
    #   location <- paste0(basename(attr(ref, "srcfile")$filename), "#", ref[1], ":1")
    # }

    # start_test = function(context, test) {
    #   cat("\ntest?: ", context, sep = "\n")
    #   if (is.null(context)) {
    #     context(context_name(self$file_name))
    #   }
    # }

  ),

  private = list()
)

# issue_summary <- function(x) {
#   type <- expectation_type(x)

#   if (is.null(x$srcref)) {
#     loc <- "???"
#   } else {
#     filename <- attr(x$srcref, "srcfile")$filename
#     loc <- paste0(basename(filename), ":", x$srcref[1])
#   }

#   header <- paste0(loc, ": ", colourise(type, type), ": ", x$test)

#   paste0(
#     crayon::bold(header), "\n",
#     format(x)
#   )
# }

# expectation_type <- function(exp) {
#   stopifnot(is.expectation(exp))
#   gsub("^expectation_", "", class(exp)[[1]])
# }

# expectation_success <- function(exp) {
#   expectation_type(exp) == "success"
# }

# expectation_failure <- function(exp) {
#   expectation_type(exp) == "failure"
# }

# expectation_error <- function(exp) {
#   expectation_type(exp) == "error"
# }

# expectation_skip <- function(exp) {
#   expectation_type(exp) == "skip"
# }

# expectation_warning <- function(exp) {
#   expectation_type(exp) == "warning"
# }

# expectation_broken <- function(exp) {
#   expectation_failure(exp) || expectation_error(exp)
# }
# expectation_ok <- function(exp) {
#   expectation_type(exp) %in% c("success", "warning")
# }
