#' Coerce names, etc. to cassettes
#'
#' @export
#' @param x Input, a cassette name (character), or something that
#' can be coerced to a cassette
#' @param ... further arguments passed on to [cassettes()] or
#' [read_cassette_meta()
#' @return a cassette of class `Cassette`
#' @examples \dontrun{
#' vcr_configure(dir = tempfile())
#' insert_cassette("foobar")
#' cassettes(on_disk = FALSE)
#' cassettes(on_disk = TRUE)
#' as.cassette("foobar", on_disk = FALSE)
#' eject_cassette() # eject the current cassette
#'
#' # cleanup
#' unlink(file.path(tempfile(), "foobar.yml"))
#' }
as.cassette <- function(x, ...) UseMethod("as.cassette")

#' @export
as.cassette.default <- function(x, ...) {
  stop("no 'as.cassette' method for ", class(x), call. = FALSE)
}

#' @export
as.cassette.cassette <- function(x, ...) x

#' @export
as.cassette.character <- function(x, ...) {
  cassettes(...)[[x]]
}

#' @export
as.cassette.cassettepath <- function(x, ...) read_cassette_meta(x, ...)

#' @export
as.cassette.list <- function(x, ...) lapply(x, as.cassette, ...)

#' Coerce to a cassette path
#'
#' @export
#' @rdname as.cassette
as.cassettepath <- function(x) UseMethod("as.cassettepath")

#' @export
as.cassettepath.character <- function(x) {
  if (file.exists(x)) {
    structure(x, class = "cassettepath")
  } else {
    stop("Path not found", call. = FALSE)
  }
}

#' @export
print.cassettepath <- function(x, ...) cat(paste0("<cassette path>"), x[[1]])
