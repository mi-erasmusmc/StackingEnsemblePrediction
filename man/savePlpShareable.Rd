% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SaveLoadPlp.R
\name{savePlpShareable}
\alias{savePlpShareable}
\title{Save the plp result as json files and csv files for transparent sharing}
\usage{
savePlpShareable(result, saveDirectory, minCellCount = 10)
}
\arguments{
\item{result}{An object of class runPlp with development or validation results}

\item{saveDirectory}{The directory the save the results as csv files}

\item{minCellCount}{Minimum cell count for the covariateSummary and certain evaluation results}
}
\description{
Save the plp result as json files and csv files for transparent sharing
}
\details{
Saves the main results json/csv files (these files can be read by the shiny app)
}
