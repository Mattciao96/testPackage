#' Split a string into substrings using a specified delimiter
#'
#' This function splits a string into substrings using a specified delimiter.
#'
#' @param x A character vector containing the strings to split.
#' @param split A character string specifying the delimiter to use for splitting.
#'
#' @return A character vector containing the first element of the result of splitting \code{x} by \code{split}.
#'
#' @examples
#' strsplit1("hello world", " ")
#'
#' @export
strsplit1 <- function(x, split) {
  strsplit(x, split = split)[[1]]
}
