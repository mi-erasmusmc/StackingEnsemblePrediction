% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RunPlpHelpers.R
\name{createExecuteSettings}
\alias{createExecuteSettings}
\title{Creates list of settings specifying what parts of runPlp to execute}
\usage{
createExecuteSettings(
  runSplitData = F,
  runSampleData = F,
  runfeatureEngineering = F,
  runPreprocessData = F,
  runModelDevelopment = F,
  runCovariateSummary = F
)
}
\arguments{
\item{runSplitData}{TRUE or FALSE whether to split data into train/test}

\item{runSampleData}{TRUE or FALSE whether to over or under sample}

\item{runfeatureEngineering}{TRUE or FALSE whether to do feature engineering}

\item{runPreprocessData}{TRUE or FALSE whether to do preprocessing}

\item{runModelDevelopment}{TRUE or FALSE whether to develop the model}

\item{runCovariateSummary}{TRUE or FALSE whether to create covariate summary}
}
\value{
list with TRUE/FALSE for each part of runPlp
}
\description{
Creates list of settings specifying what parts of runPlp to execute
}
\details{
define what parts of runPlp to execute
}
