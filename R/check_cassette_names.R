#' Check cassette names
#'
#' @export
#' @param pattern (character) regex pattern for file paths to check.
#' this is done inside of `tests/testthat/`. default: "test-"
#' @param behavior (character) "stop" (default) or "warning". if "warning",
#' we use `immediate.=TRUE` so the warning happens at the top of your
#' tests rather than you seeing it after tests have run (as would happen
#' by default)
#' @param allowed_duplicates (character) cassette names that can be duplicated
#' @includeRmd man/rmdhunks/cassette-names.Rmd details

check_cassette_names <- function(pattern = "test-", behavior = "stop",
  allowed_duplicates = NULL) {

  assert(allowed_duplicates, "character")
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
  if (!is.null(allowed_duplicates)) {
    cnms <- cnms[!cnms %in% allowed_duplicates]
  }
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
