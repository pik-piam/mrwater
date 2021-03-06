% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotReturnToIrrigation.R
\name{plotReturnToIrrigation}
\alias{plotReturnToIrrigation}
\title{plotReturnToIrrigation}
\usage{
plotReturnToIrrigation(
  y_axis_range,
  x_axis,
  scenario,
  lpjml,
  selectyears,
  climatetype,
  EFRmethod,
  rankmethod,
  yieldcalib,
  allocationrule,
  thresholdtype,
  gainthreshold,
  irrigationsystem,
  avlland_scen,
  cropmix,
  com_ag,
  multicropping,
  potential_wat = TRUE
)
}
\arguments{
\item{y_axis_range}{range of y-axis (monetary irrigation gain) to be depicted on the curve}

\item{x_axis}{x_axis type to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc" "wat_tot_ww" "wat_tot_wc"}

\item{scenario}{non-agricultural water use scenario to be displayed in plot}

\item{lpjml}{LPJmL version required for respective inputs: natveg or crop}

\item{selectyears}{years for which irrigatable area is calculated}

\item{climatetype}{Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"}

\item{EFRmethod}{EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)}

\item{rankmethod}{method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)}

\item{yieldcalib}{FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)}

\item{allocationrule}{Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")}

\item{thresholdtype}{Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha}

\item{gainthreshold}{Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)}

\item{irrigationsystem}{Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")}

\item{avlland_scen}{Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.}

\item{cropmix}{cropmix for which irrigation yield improvement is calculated
can be selection of proxycrop(s) for calculation of average yield gain
or hist_irrig or hist_total for historical cropmix}

\item{com_ag}{if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)}

\item{multicropping}{Multicropping activated (TRUE) or not (FALSE)}

\item{potential_wat}{if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used}
}
\value{
magpie object in cellular resolution
}
\description{
plot monetary yield gain achieved on irrigated area (USD) or irrigatable area (Mha) dependent on water accessibility
}
\examples{
\dontrun{
plotReturnToIrrigation(y_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
}

}
\author{
Felicitas Beier
}
