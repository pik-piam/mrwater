% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRiverNaturalFlows.R
\name{calcRiverNaturalFlows}
\alias{calcRiverNaturalFlows}
\title{calcRiverNaturalFlows}
\usage{
calcRiverNaturalFlows(selectyears, lpjml, climatetype)
}
\arguments{
\item{selectyears}{Years to be returned
(Note: does not affect years of harmonization or smoothing)}

\item{lpjml}{LPJmL version used}

\item{climatetype}{Switch between different climate scenarios
or historical baseline "GSWP3-W5E5:historical"}
}
\value{
magpie object in cellular resolution
}
\description{
This function calculates natural discharge for the river routing
             derived from inputs from LPJmL
}
\examples{
\dontrun{
calcOutput("RiverNaturalFlows", aggregate = FALSE)
}

}
\author{
Felicitas Beier, Jens Heinke
}
