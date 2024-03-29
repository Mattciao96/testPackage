#' Match species names to their corresponding accepted names in the Index fungorum database
#'
#' Given a list of species names, this function matches each name to its corresponding accepted name in the Index fungorum database. The function uses the ITALIC API to perform the matching. The function can also accept optional arguments to match at the subspecies, variety, form, or cultivar level.
#'
#' @param sp_names a character vector of species names to be matched
#' @param subsp_marks an optional character vector of markers to match at the subspecies level
#' @param var_marks an optional character vector of markers to match at the variety level
#' @param form_marks an optional character vector of markers to match at the form level
#' @param cv_marks an optional character vector of markers to match at the cultivar level
#'
#' @return a list of matched names, with the original names as the names of the list and the matched names as the values
#'
#' @examples
#' matchLicNameIndexFungorum(c("Lactuca sativa", "Triticum aestivum"))
#'
#' @import httr
#' @import jsonlite
#'
#' @export
matchLicNameIndexFungorum <-function(sp_names, subsp_marks = c(), var_marks = c(), form_marks = c(), cv_marks = c()) {

  # sp_names must be a vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_string must be a string or a vector")
  } else if (is.character(sp_names)) {
    sp_names = strToVector(sp_names)
  }

  # create a vector with only unique species names
  unique_sp_names <- unique(sp_names)

  # for each unique name call the match api the result is put in a dataframe
  progress_bar <- defineProgressBar(length(unique_sp_names))
  for (i in 1:length(unique_sp_names)) {
    # these parameters are used to retry the iteration after a 429 code

    success <- FALSE
    while (!success) {

      sp_name <- unique_sp_names[i];
      #print(sp_name)

      url <- "https://italic.units.it/api/matchif"
      headers <- c('Content-Type' = 'application/json')
      body <-
        list(
          'sp' = sp_name,
          'subsp-mark' = subsp_marks,
          'var-mark' = var_marks,
          'form-mark' = form_marks,
          'cv-mark' = cv_marks
        )

      # Send POST request to API
      response <-
        POST(url,
             body = jsonlite::toJSON(body),
             encode = "json",
             add_headers(headers))

      # Deal with api errors
      # 500 server not available (blocks the function)
      # 429 API usage limit exceeded (the function should wait 60 sec)
      if (response$status_code == 500) {
        stop("Impossible to connect to the server, please try again later")
      } else if (response$status_code == 429) {
        # wait the end of the api cooldown and retry
        waitApiCooldown()
      } else if (response$status_code == 200) {
        success <- TRUE
      } else {
        stop("An unknown error occurred, please try again later")
      }

    }

    # If status_code = 200 everything is fine

    #progress bar
    utils::setTxtProgressBar(progress_bar, i)
    # Extract response content
    data = fromJSON(rawToChar(response$content))

    input = as.data.frame(data[1])
    match = data[2]
    match <- match$match
    result = cbind(input, match)
    if (i == 1) {
      result_merged <- result
    } else {
      result_merged <- rbind(result_merged, result)
    }

  }

  # at the end of the cycle, the original array is rebuilt
  ordered_dataframe <- reconstructDataFrameWithOriginalOrder(sp_names, result_merged, 1)
  return(ordered_dataframe)

}
