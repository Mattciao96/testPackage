#' Wait for a specified number of seconds
#'
#' This function waits for a specified number of seconds before continuing.
#'
#' @param seconds The number of seconds to wait (default = 60)
#' @keywords wait time
waitApiCooldown <- function(seconds = 10) {
  #print('waiting 60 seconds to reuse the API')
  Sys.sleep(seconds)
}
