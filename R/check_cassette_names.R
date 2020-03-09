#' Check cassette names
#'
#' @export
#' @param pattern (character) regex pattern for file paths to check.
#' this is done inside of `tests/testthat/`. default: "test-"
#' @param behavior (character) "stop" (default) or "warning". if "warning",
#' we use `immediate.=TRUE` so the warning happens at the top of your
#' tests rather than you seeing it after tests have run (as would happen
#' by default)
#' @details This function is meant to be run during your tests, from a
#' `helper-pkgname.R` file inside the `tests/testthat` directory. It only
#' checks that cassette names are not duplicated. A helper function 
#' `check_cassette_name()` runs inside [insert_cassette()] that checks
#' that cassettes do not have: spaces, file extensions, unaccepted
#' characters (slashes)
#' @section Cassette names:
#' - Should be meaningful so that it's obvious to you what test/function
#' they relate to. Meaningful names are important so that you can quickly
#' determine to what test file or test block a cassette belongs. Note that
#' vcr cannot check that your cassette names are meaningful.
#' - Should not be duplicated. Duplicated cassette names would lead to
#' a test using the wrong cassette.
#' - Should not have spaces. Spaces can lead to problems in using file paths.
#' - Should not include a file extension. vcr handles file extensions for
#' the user.
#' - Should not have illegal characters that can lead to problems in using
#' file paths: '/', '?', '<', '>', '\\', ':', '*', '|', and '\"'
#' - Should not have control characters, e.g., `\n`
#' - Should not have just dots, e.g,. `.` or `..`
#' - Should not have Windows reserved words, e.g, `com1`
#' - Should not have trailing dots
#' - Should not be longer than 255 characters
check_cassette_names <- function(pattern = "test-", behavior = "stop") {
  files <- list.files(".", pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return()
  cassette_names <- function(x) {
    tmp <- parse(x, keep.source = TRUE)
    df <- utils::getParseData(tmp)
    row.names(df) = NULL
    z <- as.numeric(row.names(df[df$text == "use_cassette", ])) + 2
    gsub("\"", "", df[z, "text"])
  }
  nms <- stats::setNames(lapply(files, cassette_names), files)
  cnms <- unname(unlist(nms))
  if (any(duplicated(cnms))) {
    dups <- unique(cnms[duplicated(cnms)])
    fdups <- c()
    for (i in seq_along(dups)) {
      matched <- lapply(nms, function(w) dups[i] %in% w)
      fdups[i] <- sprintf("%s (found in %s)", dups[i],
        paste0(basename(names(nms[unlist(matched)])), collapse = ", ")
      )
    }
    mssg <- c("you should not have duplicated cassette names:",
      paste0("\n    ", paste0(fdups, collapse = "\n    ")))
    switch(behavior,
      stop = stop(mssg, call. = FALSE),
      warning = warning(mssg, call. = FALSE, immediate. = TRUE)
    )
  }
}
