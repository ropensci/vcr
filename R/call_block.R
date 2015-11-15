#' Record a call to a block of code
#'
#' @examples \dontrun{
#' cassette <- cassettes()[[3]]
#' call_block(cassette, {
#'   GET("http://httpbin.org/get")
#' })
#' }

call_block <- function(cassette, block) {
  call_block_(cassette, substitute(block))
  # substitute(block)
  # invisible(out)
}

call_block_ <- function(cassette, block){
  out <- eval(block)
  write_cassette(cassette, out)
  return( out )
}

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
  # cat("headers: \n", file = file)
  # cat(yaml::as.yaml(x$headers, indent = 4), file = file, append = TRUE)
  cat("   body:", file = file, append = TRUE)
  cat("\n",
      strwrap(
        gsub('\"', '\\\\"',
          gsub("\\n", "\\\\n",
             sprintf("  string: '%s'", content(x, as = "text"))
        )),
      indent = 5, exdent = 10),
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
