#' Convert string to vector with only the string
#'
#' This function takes a string as input and returns a vector with only the string in it.
#'
#' @param string A character string to be converted to a vector
#'
#' @return A character vector containing only the input string
#' @keywords internal
#'
strToVector <- function(string) {
  return(c(string))
}
