#' @export
print.cassette <- function(x, ...){
  cat(paste0("<cassette> ", x$name), sep = "\n")
  cat(paste0("  Record method: ", x$record), sep = "\n")
  cat(paste0("  Serialize with: ", x$serialize_with), sep = "\n")
  cat(paste0("  Persist with: ", x$persist_with), sep = "\n")
  cat(paste0("  update_content_length_header: ", x$update_content_length_header), sep = "\n")
  cat(paste0("  decode_compressed_response: ", x$decode_compressed_response), sep = "\n")
  cat(paste0("  allow_playback_repeats: ", x$allow_playback_repeats), sep = "\n")
  cat(paste0("  allow_unused_http_interactions: ", x$allow_unused_http_interactions), sep = "\n")
  cat(paste0("  exclusive: ", x$exclusive), sep = "\n")
  cat(paste0("  preserve_exact_body_bytes: ", x$preserve_exact_body_bytes), sep = "\n")
}
