% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRiverWatReserved.R
\name{calcRiverWatReserved}
\alias{calcRiverWatReserved}
\title{calcRiverWatReserved}
\usage{
calcRiverWatReserved(
  selectyears,
  iniyear,
  lpjml,
  climatetype,
  EFRmethod,
  accessibilityrule,
  com_ag
)
}
\arguments{
\item{selectyears}{Years to be returned
(Note: does not affect years of harmonization or smoothing)}

\item{iniyear}{Initialization year of irrigation system}

\item{lpjml}{LPJmL version used}

\item{climatetype}{Switch between different climate scenarios
or historical baseline "GSWP3-W5E5:historical"}

\item{EFRmethod}{EFR method used including selected strictness of EFRs
(Smakhtin:good, VMF:fair)}

\item{accessibilityrule}{Method used: Quantile method (Q) or Coefficient of Variation (CV)
combined with scalar value defining the strictness of accessibility restriction:
discharge that is exceeded x percent of the time on average throughout a year
(Qx, e.g. Q75: 0.25, Q50: 0.5)
or base value for exponential curve separated by : (CV:2)}

\item{com_ag}{If TRUE: currently already irrigated areas in initialization
         year are reserved for irrigation,
if FALSE: no irrigation areas are reserved (full irrigation potential)}
}
\value{
magpie object in cellular resolution
}
\description{
This function calculates water that is reserved for the environment,
             other human uses or is inaccessible and cannot be withdrawn for
             irrigation
}
\examples{
\dontrun{
calcOutput("RiverWatReserved", aggregate = FALSE)
}

}
\author{
Felicitas Beier, Jens Heinke
}
