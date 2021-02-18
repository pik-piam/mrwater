% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRiverNaturalFlows22.R
\name{calcRiverNaturalFlows22}
\alias{calcRiverNaturalFlows22}
\title{calcRiverNaturalFlows_magpie_magpie22}
\usage{
calcRiverNaturalFlows22(
  selectyears = "all",
  version = "LPJmL4",
  climatetype = "HadGEM2_ES:rcp2p6:co2",
  time = "spline",
  averaging_range = NULL,
  dof = 4,
  harmonize_baseline = "CRU_4",
  ref_year = "y2015"
)
}
\arguments{
\item{selectyears}{Years to be returned (Note: does not affect years of harmonization or smoothing)}

\item{version}{Switch between LPJmL4 and LPJmL5}

\item{climatetype}{Switch between different climate scenarios (default: "CRU_4")}

\item{time}{Time smoothing: average, spline or raw (default)}

\item{averaging_range}{only specify if time=="average": number of time steps to average}

\item{dof}{only specify if time=="spline": degrees of freedom needed for spline}

\item{harmonize_baseline}{FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)}

\item{ref_year}{Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)}
}
\value{
magpie object in cellular resolution
}
\description{
This function calculates natural discharge and associated lake evaporation for the river routing derived from inputs from LPJmL
}
\examples{
\dontrun{ calcOutput("RiverNaturalFlows_magpie22", aggregate = FALSE) }

}
\author{
Felicitas Beier, Jens Heinke
}