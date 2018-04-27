get_method <- function(x) {
  x <- as.character(x)
  tmp <- grep(
    "(get)$|(post)$|(put)$|(delete)$|(options)$|(patch)$|(head)$",
    tolower(x), value = TRUE)
  tmp <- sub("httr::", "", tmp)
  if (length(tmp) == 0) NULL else tmp
}

is_url <- function(x) {
  grepl(
    "https?://", x, ignore.case = TRUE) ||
    grepl("localhost:[0-9]{4}", x, ignore.case = TRUE
    )
}

get_uri <- function(x) {
  x <- as.character(x)
  tmp <- x[vapply(x, is_url, logical(1))]
  if (length(tmp) == 0) NULL else tmp
}

get_host <- function(x) {
  eval(parse(text = x))(x)$hostname
}

get_path <- function(x) {
  eval(parse(text = vcr_c$uri_parser))(x)$path
}

get_query <- function(x) {
  if ("query" %in% names(x)) {
    x[["query"]]
  } else {
    NULL
  }
}

get_body <- function(x) {
  if ("body" %in% names(x)) {
    x[["body"]]
  } else {
    NULL
  }
}

parseurl <- function(x) {
  tmp <- urltools::url_parse(x)
  tmp <- as.list(tmp)
  if (!is.na(tmp$parameter)) {
    tmp$parameter <- sapply(strsplit(tmp$parameter, "&")[[1]], function(z) {
      zz <- strsplit(z, split = "=")[[1]]
      as.list(stats::setNames(zz[2], zz[1]))
    }, USE.NAMES = FALSE)
  }
  tmp
}
