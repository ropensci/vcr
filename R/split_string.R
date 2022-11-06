#' split string every N characters
#' @param str (character) a string
#' @param length (integer) number of characters to split by
#' @examples \dontrun{
#' str = "XOVEWVJIEWNIGOIWENVOIWEWVWEW"
#' str_splitter(str, 5)
#' str_splitter(str, 5L)
#' }
str_splitter <- function(str, length) {
	gsub(sprintf("(.{%s})", length), "\\1 ", str)
}
