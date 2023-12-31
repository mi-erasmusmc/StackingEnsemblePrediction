% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunMultiplePlp.R
\name{loadPlpAnalysesJson}
\alias{loadPlpAnalysesJson}
\title{Load the multiple prediction json settings from a file}
\usage{
loadPlpAnalysesJson(jsonFileLocation)
}
\arguments{
\item{jsonFileLocation}{The location of the file 'predictionAnalysisList.json' with the modelDesignList}
}
\description{
Load the multiple prediction json settings from a file
}
\details{
This function interprets a json with the multiple prediction settings and creates a list 
that can be combined with connection settings to run a multiple prediction study
}
\examples{
\dontrun{
modelDesignList <- loadPlpAnalysesJson('location of json settings')$analysis
}

}
