#' @export
print.cassette <- function(x, ...){
  cat(paste0("<cassette> ", x$name), sep = "\n")
  cat(paste0("  Recorded at: ", if (!is.null(x$recorded_at))
    x$recorded_at), sep = "\n")
  cat(paste0("  Recorded with: ", if (!is.null(x$recorded_at))
    x$recorded_with), sep = "\n")
}
