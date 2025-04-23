#' Extract request body
#'
#' @details S3 methods and the packages they are used for
#' - request: `httr`
#' - list: `crul`
#' - httr2_request: `httr2`
#' @keywords internal
take_body <- function(x) {
  UseMethod("take_body")
}

#' @keywords internal
take_body.request <- function(x) {
  if (is_body_empty(x)) {
    return(NULL)
  }

  if (!is.null(x$fields)) {
    # multipart body
    tmp <- x$fields
  } else if (!is.null(x$options$postfields) && is.raw(x$options$postfields)) {
    # json/raw-encoded body
    tmp <- rawToChar(x$options$postfields)
  } else if (!is.null(x$options$postfieldsize_large)) {
    # upload not in a list
    # seems like we can't get the file path anyway from the request
    # in both crul and httr - so may be stuck with this
    tmp <- paste0("upload, file size: ", x$options$postfieldsize_large)
  } else {
    # unknown, fail out
    cli::cli_abort("couldn't fetch request body; please file an issue")
  }
  if (inherits(tmp, "raw")) rawToChar(tmp) else tmp
}

#' @keywords internal
take_body.list <- take_body.request

#' @note adapted from httr2:::req_body_get
#' @keywords internal
take_body.httr2_request <- function(x) {
  if (is.null(x$body)) {
    return("")
  }
  switch(
    x$body$type,
    raw = {
      # httr2::req_body_raw allows raw or string
      if (is_raw(x$body$data)) rawToChar(x$body$data) else x$body$data
    },
    form = {
      data <- x$body$data # need to put back unobfuscate?
      httr2_url_build(data)
    },
    json = rlang::exec(jsonlite::toJSON, x$body$data, !!!x$body$params),
    # FIXME: for now take the file path - would be good to get what would
    # be sent in a real request
    "raw-file" = x$body$data,
    multipart = x$body$data,
    cli::cli_abort("Unsupported request body type {.str {x$body$type}}.")
  )
}

httr2_url_build <- function(data) {
  replace_me <- "http://x"
  tmp <- crul::url_build(replace_me, query = data)
  tmp <- sub(replace_me, "", tmp)
  sub("\\/\\?", "", tmp)
}

is_body_empty <- function(x) {
  is.null(x$fields) &&
    (is.null(x$options$postfieldsize) || x$options$postfieldsize == 0L)
}
