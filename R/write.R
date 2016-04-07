write_cassette <- function(cassette, result){
  file <- get_cassette_data_paths()[cassette$name][[1]]
  write_yaml(result, file)
}

write_yaml <- function(x, file){
  cat("---\nhttp_interactions:\n- request:", sep = "\n", file = file)
  cf(sprintf("method: %s", x$request$method), file)
  cf(sprintf("uri: %s", x$url), file)
  forwrite("body:", x$request$opts, file)
  cat("  response:", file = file, append = TRUE, sep = "\n")
  cf(sprintf("status_code: %s", x$status_code), file)
  forwrite("headers:", x$headers, file)
  cat("   body:", file = file, append = TRUE)
  str <- base64enc::base64encode(httr::content(x, as = "raw", encoding = "UTF-8"))
  ncar <- nchar(str)
  cat("\n",
      strwrap(
        paste0(substring(str, seq(1, ncar, 60), seq(60, ncar, 60)), collapse = "\n"),
        width = 60, indent = 6, exdent = 6
      ),
      file = file, fill = 80, append = TRUE
  )
  cat(sprintf("   recorded_at: %s", Sys.time()), file = file, sep = "\n", append = TRUE)
  cat(sprintf("   recorded_with: %s", packageVersion("vcr")), file = file, sep = "\n", append = TRUE)
}

forwrite <- function(name, x, file){
  cf(name, file)
  for (i in seq_along(x)) {
    cf(sprintf("  %s: '%s'", names(x[i]), x[[i]]), file)
  }
}

cf <- function(x, f){
  cat(paste0("   ", x), sep = "\n", file = f, append = TRUE)
}
