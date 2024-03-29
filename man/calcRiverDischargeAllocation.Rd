% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/calcRiverDischargeAllocation.R
\name{calcRiverDischargeAllocation}
\alias{calcRiverDischargeAllocation}
\title{calcRiverDischargeAllocation}
\usage{
calcRiverDischargeAllocation(
  lpjml,
  climatetype,
  selectyears,
  efrMethod,
  accessibilityrule,
  transDist,
  rankmethod,
  yieldcalib,
  allocationrule,
  gainthreshold,
  irrigationsystem,
  iniyear,
  landScen,
  cropmix,
  comAg,
  multicropping
)
}
\arguments{
\item{lpjml}{LPJmL version used}

\item{climatetype}{Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"}

\item{selectyears}{Years to be returned
(Note: does not affect years of harmonization or smoothing)}

\item{efrMethod}{EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)}

\item{accessibilityrule}{Method used: Quantile method (Q) or Coefficient of Variation (CV)
combined with scalar value defining the strictness of accessibility restriction:
discharge that is exceeded x percent of the time on average throughout a year
(Qx, e.g. Q75: 0.25, Q50: 0.5)
or base value for exponential curve separated by : (CV:2)}

\item{transDist}{Water transport distance allowed to fulfill locally
unfulfilled water demand}

\item{rankmethod}{Rank and optimization method consisting of
Unit according to which rank is calculated:
USD_ha (USD per hectare) for relative area return, or
USD_m3 (USD per cubic meter) for relative volumetric return;
USD for absolute return (total profit);
Price aggregation:
"GLO" for global average prices, or
"ISO" for country-level prices
and boolean indicating fullpotential (TRUE, i.e. cell
receives full irrigation requirements in total area)
or reduced potential (FALSE, reduced potential of cell
receives at later stage in allocation algorithm);
separated by ":"}

\item{yieldcalib}{If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
     Also needs specification of refYields, separated by ":".
     Options: FALSE (for single cropping analyses) or
              "TRUE:actual:irrig_crop" (for multiple cropping analyses)
If FALSE: uncalibrated LPJmL yields are used}

\item{allocationrule}{Rule to be applied for river basin discharge allocation
("optimization" or "upstreamfirst")}

\item{gainthreshold}{Threshold of yield improvement potential
(in USD per hectare)}

\item{irrigationsystem}{Irrigation system to be used for river basin discharge
allocation algorithm ("surface", "sprinkler", "drip", "initialization")}

\item{iniyear}{Initialization year of irrigation system}

\item{landScen}{Land availability scenario consisting of two parts separated by ":":
1. available land scenario (currCropland, currIrrig, potCropland)
2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
   e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
   or NA for no protection).
For case of no land protection select "NA" in second part of argument
or do not specify second part of the argument}

\item{cropmix}{Selected cropmix (options:
"hist_irrig" for historical cropmix on currently irrigated area,
"hist_total" for historical cropmix on total cropland,
or selection of proxycrops)}

\item{comAg}{if TRUE: the currently already irrigated areas
         in initialization year are reserved for irrigation,
if FALSE: no irrigation areas reserved (irrigation potential)}

\item{multicropping}{Multicropping activated (TRUE) or not (FALSE) and
Multiple Cropping Suitability mask selected
(mask can be:
"none": no mask applied (only for development purposes)
"actual:total": currently multicropped areas calculated from total harvested areas
                and total physical areas per cell from LandInG
"actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
"actual:irrig_crop" (crop- and irrigation-specific) "total"
"potential:endogenous": potentially multicropped areas given
                        temperature and productivity limits
"potential:exogenous": potentially multicropped areas given
                       GAEZ suitability classification)
(e.g. TRUE:actual:total; TRUE:none; FALSE)}
}
\value{
magpie object in cellular resolution
}
\description{
This function distributes surplus basin discharge after the
             previous river routings following certain management assumptions
}
\examples{
\dontrun{
calcOutput("RiverDischargeAllocation", aggregate = FALSE)
}

}
\author{
Felicitas Beier, Jens Heinke
}
