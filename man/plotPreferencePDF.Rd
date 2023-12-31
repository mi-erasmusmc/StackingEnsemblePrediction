% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Plotting.R
\name{plotPreferencePDF}
\alias{plotPreferencePDF}
\title{Plot the preference score probability density function, showing prediction overlap between true and false cases
#'}
\usage{
plotPreferencePDF(
  plpResult,
  typeColumn = "evaluation",
  saveLocation = NULL,
  fileName = "plotPreferencePDF.png"
)
}
\arguments{
\item{plpResult}{A plp result object as generated using the \code{\link{runPlp}} function.}

\item{typeColumn}{The name of the column specifying the evaluation type}

\item{saveLocation}{Directory to save plot (if NULL plot is not saved)}

\item{fileName}{Name of the file to save to plot, for example
'plot.png'. See the function \code{ggsave} in the ggplot2 package for
supported file formats.}
}
\value{
A ggplot object. Use the \code{\link[ggplot2]{ggsave}} function to save to file in a different
format.
}
\description{
Plot the preference score probability density function, showing prediction overlap between true and false cases
#'
}
\details{
Create a plot showing the preference score probability density function, showing prediction overlap between true and false cases
#'
}
