#' Check cassette names
#'
#' @export
#' @param pattern (character) regex pattern for file paths to check. 
#' this is done inside of `tests/testthat/`. default: "test-"
#' @param behavior (character) "stop" (default) or "warning". if "warning",
#' we use `immediate.=TRUE` so the warning happens at the top of your
#' tests rather than you seeing it after tests have run (as would happen
#' by default)
#' @details This function is meant to be run in a `helper-pkgname.R` file
#' inside the `tests/testthat` directory.
check_cassette_names <- function(pattern = "test-", behavior = "stop") {
  files <- list.files(".", pattern = pattern, full.names = TRUE)
  if (length(files) == 0) return()
  cassette_names <- function(x) {
    tmp <- parse(x, keep.source = TRUE)
    df <- getParseData(tmp)
    row.names(df) = NULL
    z <- as.numeric(row.names(df[df$text == "use_cassette", ])) + 2
    gsub("\"", "", df[z, "text"])
  }
  nms <- unlist(lapply(files, cassette_names))
  if (any(duplicated(nms))) {
    mssg <- c("you should not have duplicated cassette names:",
      paste0("\n    ", paste0(nms[duplicated(nms)], collapse = "\n    ")))
    switch(behavior,
      stop = stop(mssg, call. = FALSE),
      warning = warning(mssg, call. = FALSE, immediate. = TRUE)
    )
  }
}
