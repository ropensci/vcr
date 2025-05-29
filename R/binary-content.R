has_text_content <- function(headers) {
  idx <- match("content-type", tolower(names(headers)))
  if (is.na(idx)) {
    # If no content type, assume it's binary
    FALSE
  } else {
    is_text_type(headers[[idx]])
  }
}

is_text_type <- function(content_type) {
  parsed <- parse_content_type(content_type)
  if (parsed$type == "text") {
    return(TRUE)
  }

  # Otherwise assume it's binary, unless it's one of a handful of special cases
  special_cases <- c(
    "application/xml",
    "application/x-www-form-urlencoded",
    "application/json",
    "application/ld+json",
    "multipart/form-data"
  )
  base_type <- paste0(parsed$type, "/", parsed$subtype)
  base_type %in% special_cases
}

# Copied from httr2::parse_content_type
parse_content_type <- function(x) {
  regex <- "^(?<type>application|audio|font|example|image|message|model|multipart|text|video)/(?<subtype>(?:(?:vnd|prs|x)\\.)?(?:[^+;])+)(?:\\+(?<suffix>(?:[^;])+))?(?:;(?<parameters>(?:.)+))?$"
  if (!grepl(regex, x, perl = TRUE)) {
    out <- list(
      type = "",
      subtype = "",
      suffix = ""
    )
    return(out)
  }

  match_object <- regexec(regex, x, perl = TRUE)
  match <- regmatches(x, match_object)[[1]]
  list(
    type = match[[2]],
    subtype = match[[3]],
    suffix = if (match[[4]] != "") match[[4]] else ""
  )
}
