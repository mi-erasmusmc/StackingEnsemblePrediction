% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Recalibration.R
\name{recalibratePlp}
\alias{recalibratePlp}
\title{recalibratePlp}
\usage{
recalibratePlp(
  prediction,
  analysisId,
  typeColumn = "evaluationType",
  method = c("recalibrationInTheLarge", "weakRecalibration")
)
}
\arguments{
\item{prediction}{A prediction dataframe}

\item{analysisId}{The model analysisId}

\item{typeColumn}{The column name where the strata types are specified}

\item{method}{Method used to recalibrate ('recalibrationInTheLarge' or 'weakRecalibration' )}
}
\value{
An object of class \code{runPlp} that is recalibrated on the new data
}
\description{
Train various models using a default parameter gird search or user specified parameters
}
\details{
The user can define the machine learning model to train (regularised logistic regression, random forest,
gradient boosting machine, neural network and )
}
