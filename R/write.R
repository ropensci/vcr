write_cassette <- function(cassette, result){
  file <- get_cassette_data_paths()[cassette$name][[1]]
  write_yaml(result, file)
}

write_yaml <- function(x, file){
  write_header(file)
  lapply(x, write_interactions, file = file)
}

write_header <- function(file) {
  cat("http_interactions:", sep = "\n", file = file)
  # if (!file.exists(file)) {
  #   cat("http_interactions:", sep = "\n", file = file)
  # } else {
  #   if (length(readLines(file, n = 1)) == 0) {
  #     cat("http_interactions:", sep = "\n", file = file)
  #   }
  # }
}

write_interactions <- function(x, file) {
  cat(yaml::as.yaml(
    list(
      list(
        request = list(
          method = x$request$method,
          uri = x$request$uri,
          body = list(
            encoding = "",
            string = get_body(x$request$body)
          ),
          headers = x$request$headers
        ),
        response = list(
          status = x$response$status,
          headers = x$response$headers,
          body = list(
            # encoding = "",
            encoding = encoding_guess(x$response$body),
            # FIXME - be able to toggle whether to base64encode or not
            string = if (vcr_c$preserve_exact_body_bytes) {
              base64enc::base64encode(charToRaw(get_body(x$response$body)))
            } else {
              get_body(x$response$body)
            }
          )
        ),
        recorded_at = paste0(format(Sys.time(), tz = "GMT"), " GMT"),
        recorded_with = paste0("vcr/", utils::packageVersion("vcr"))
      )
    )
  ), file = file, append = TRUE)
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

get_body <- function(x) {
  if (is.null(x)) '' else x
}

strex <- function(string, pattern) {
  regmatches(string, regexpr(pattern, string))
}

encoding_guess <- function(x, force_guess = FALSE) {
  if (vcr_c$preserve_exact_body_bytes && !force_guess) return("ASCII-8BIT")
  enc <- Encoding(x)
  if (enc == "unknown") {
    message("encoding couldn't be detected; assuming UTF-8")
  }
  return("UTF-8")
}

# write_interactions <- function(x, file){
#   cat("- request:", sep = "\n", file = file, append = TRUE)
#   cf(sprintf("method: %s", x$request$method), file)
#   cf(sprintf("uri: %s", x$request$uri), file)
#   cf(sprintf("body:"), file)
#   cf(sprintf("   encoding: "), file)
#   cf(sprintf("   string: %s", get_body(x$request$body)), file)
#   forwrite("headers:", x$request$headers, file)
#   cat("  response:", file = file, append = TRUE, sep = "\n")
#   cf(sprintf("status:"), file)
#   cf(sprintf("   code: %s", strex(x$response$status$message, "[0-9]{3}")), file)
#   cf(sprintf("   message: %s", x$response$status$reason), file)
#   forwrite("headers:", x$response$headers, file)
#   cf(sprintf("body:"), file)
#   cf(sprintf("   encoding: %s", Encoding(x$response$body)), file)
#   cf("   string:", file)
#   # FIXME - we shouldn't always be base64 encoding, only when user requests it
#   str <- base64enc::base64encode(charToRaw(get_body(x$response$body)))
#   ncar <- nchar(str)
#   cat(
#       strwrap(
#         paste0(substring(str, seq(1, ncar, 60), seq(60, ncar, 60)), collapse = "\n"),
#         width = 60, indent = 10, exdent = 10
#       ),
#       file = file, fill = 80, append = TRUE
#   )
#   cat(sprintf("   recorded_at: %s", Sys.time()), file = file, sep = "\n", append = TRUE)
#   cat(sprintf("   recorded_with: %s", paste0("vcr/", utils::packageVersion("vcr"))), file = file, sep = "\n", append = TRUE)
# }
