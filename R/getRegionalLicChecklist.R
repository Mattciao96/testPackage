#' Get regional checklist from the ITALIC database
#'
#' This function sends a POST request to the API endpoint to retrieve the checklist
#' for the specified region.
#'
#' @param region A string representing the region for which to retrieve the checklist
#'
#' @return A vector containing the license checklist data
#'
#' @examples
#' # Get the license checklist for the region "Marche"
#' getRegionalLicChecklist('Marche')
#'
#' @import httr
#' @import jsonlite
#'
#' @export
getRegionalLicChecklist <-function(region) {

  # region must be a string
  if (!is.character(region)) {
    stop("sp_string must be a string")
  }

  url <- "https://italic.units.it/api/checklist"
  headers <- c('Content-Type' = 'application/json')
  body <- list('region' = region)

  # Send POST request to API
  response <-
    POST(url,
         body = jsonlite::toJSON(body, auto_unbox = TRUE), #auto_unbox to remove the generated vector
         encode = "json",
         add_headers(headers))

  # Deal with api errors
  # 500 server not available (blocks the function)
  # 429 API usage limit exceeded (the function should wait 60 sec)
  if (response$status_code == 500) {
    stop("Impossible to connect to the server, please try again later")
  } else if (response$status_code == 400) {
    error <- fromJSON(rawToChar(response$content))
    stop(error)
  } else if (response$status_code == 429) {
    # wait the end of the api cooldown and retry
    waitApiCooldown()
  } else if (response$status_code == 200) {
    success <- TRUE
  } else {
    stop("An unknown error occurred, please try again later")
  }

  # If status_code = 200 everything is fine

  # Extract response content
  #data = fromJSON(rawToChar(response$content))
  return(fromJSON(rawToChar(response$content)))
}
