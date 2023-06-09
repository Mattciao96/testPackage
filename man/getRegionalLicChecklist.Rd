% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getRegionalLicChecklist.R
\name{getRegionalLicChecklist}
\alias{getRegionalLicChecklist}
\title{Get regional checklist from the ITALIC database}
\usage{
getRegionalLicChecklist(region)
}
\arguments{
\item{region}{A string representing the region for which to retrieve the checklist}
}
\value{
A vector containing the license checklist data
}
\description{
This function sends a POST request to the API endpoint to retrieve the checklist
for the specified region.
}
\examples{
# Get the license checklist for the region "Marche"
getRegionalLicChecklist('Marche')

}
