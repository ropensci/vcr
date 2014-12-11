#' Coerce names, etc. to cassettes
#' @export
as.cassette <- function(x) UseMethod("as.cassette")

#' @export
as.cassette.cassette <- function(x) x

#' @export
as.cassette.character <- function(x) if(is_path(x)) read_cassette_meta(x) else read_cassette_meta(get_cassette_path(x))

#' @export
as.cassette.cassettepath <- function(x) read_cassette_meta(x)
# as.cassette.numeric <- function(x) read_cassette_meta(x)

#' @export
as.cassette.list <- function(x) lapply(x, as.cassette)

#' Coerce to a cassette path
#' @export
as.cassettepath <- function(x) UseMethod("as.cassettepath")

#' @export
as.cassettepath.character <- function(x) if(file.exists(x)) structure(x, class="cassettepath") else stop("Path not found")

#' @export
print.cassettepath <- function(x) cat(paste0("<cassette path>"), x[[1]])
