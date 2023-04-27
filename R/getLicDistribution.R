#' Match species names to their corresponding accepted names in the ITALIC database
#'
#' Given a list of species names, this function matches each name to its corresponding accepted name in the ITALIC database. The function uses the ITALIC API to perform the matching. The function can also accept optional arguments to match at the subspecies, variety, form, or cultivar level.
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
#' matchLicName(c("Lactuca sativa", "Triticum aestivum"))
#'
#' @import httr
#' @import jsonlite
#'
#' @export
getLicDistribution <-function(sp_names) {

  # sp_names must be a vector
  if (!is.character(sp_names) && !is.vector(sp_names)) {
    stop("sp_string must be a string or a vector")
  } else if (is.character(sp_names)) {
    sp_names = strToVector(sp_names)
  }

  unique_sp_names <- unique(sp_names)
  # for each unique name call the match api the result is put in a dataframe
  progress_bar <- defineProgressBar(length(unique_sp_names))
  warnings <- c();


  for (i in 1:length(unique_sp_names)) {
    # these parameters are used to retry the iteration after a 429 code

    success <- FALSE
    while (!success) {

      sp_name <- unique_sp_names[i];
      #print(sp_name)

      url <- "https://italic.units.it/api/distribution"
      headers <- c('Content-Type' = 'application/json')
      body <-
        list(
          'sp' = sp_name
        )

      # Send POST request to API
      response <-
        POST(url,
             body = jsonlite::toJSON(body, auto_unbox = T),
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

    }

    # If status_code = 200 everything is fine

    #progress bar
    utils::setTxtProgressBar(progress_bar, i)
    # Extract response content
    data = fromJSON(rawToChar(response$content))
    distribution <- as.data.frame(data$distribution)
    warning <- data$warnings
    # put all the errors in a vector
    if (warning != '') {
      warnings <- append(warnings, warning)
    }

    result = cbind(sp_name, distribution)



    if (i == 1) {
      result_merged <- result
    } else {
      result_merged <- rbind(result_merged, result)
    }

  }


  # throw all errors together
  if (length(warnings) != 0) {
    #message_start <- 'The following taxa are not present in the database of ITALIC'
    message_body <- paste(warnings, collapse ="\n ")
    message_end <- "Try to align scientific names to the database of ITALIC with the function matchLIcName()"


    warning(paste("\n",as.character(message_body),"\n", message_end))

  }

  # at the end of the cycle, the original array is rebuilt
  ordered_dataframe <- reconstructDataFrameWithOriginalOrder(sp_names, result_merged, 1)

  return(ordered_dataframe)

}
