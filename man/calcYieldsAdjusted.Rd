% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcYieldsAdjusted.R
\name{calcYieldsAdjusted}
\alias{calcYieldsAdjusted}
\title{calcYieldsAdjusted}
\usage{
calcYieldsAdjusted(lpjml, climatetype, iniyear, selectyears, yieldcalib)
}
\arguments{
\item{lpjml}{LPJmL version used}

\item{climatetype}{Switch between different climate scenarios or
historical baseline "GSWP3-W5E5:historical" for yields}

\item{iniyear}{Year to be used for cropland of yield calibration}

\item{selectyears}{Years to be returned by the function}

\item{yieldcalib}{FAO (LPJmL yields calibrated with current FAO yield) or
calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
smoothed_calib (not harmonized, but calibrated)}
}
\value{
magpie object in cellular resolution
}
\description{
This function returns irrigated and rainfed yields for magpie crops.
             It can either return LPJmL potential yields of different stages or
             LPJmL yields calibrated to FAO country yields
}
\examples{
\dontrun{
calcOutput("YieldsAdjusted", aggregate = FALSE)
}

}
\author{
Felicitas Beier
}
