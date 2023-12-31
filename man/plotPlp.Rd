% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plotting.R
\name{plotPlp}
\alias{plotPlp}
\title{Plot all the PatientLevelPrediction plots}
\usage{
plotPlp(plpResult, saveLocation = NULL, typeColumn = "evaluation")
}
\arguments{
\item{plpResult}{Object returned by the runPlp() function}

\item{saveLocation}{Name of the directory where the plots should be saved (NULL means no saving)}

\item{typeColumn}{The name of the column specifying the evaluation type 
(to stratify the plots)}
}
\value{
TRUE if it ran
}
\description{
Plot all the PatientLevelPrediction plots
}
\details{
Create a directory with all the plots
}
