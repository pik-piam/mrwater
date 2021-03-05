#' @title       calcRiverWaterAllocation
#' @description This function is the wrapper function of the River-Routing. It returns results from river routing (potential human uses and cellular discharge)
#'
#' @param selectyears     Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param time            Time smoothing: average, spline or raw (default)
#' @param averaging_range only specify if time=="average": number of time steps to average
#' @param dof             only specify if time=="spline": degrees of freedom needed for spline
#' @param harmonize_baseline FALSE (default): no harmonization, TRUE: if a baseline is specified here data is harmonized to that baseline (from ref_year on)
#' @param ref_year           Reference year for harmonization baseline (just specify when harmonize_baseline=TRUE)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#' @import mrmagpie
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("RiverWaterAllocation", aggregate = FALSE) }
#'

calcRiverWaterAllocation <- function(selectyears, climatetype, time, averaging_range, dof, harmonize_baseline, ref_year, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, protect_scen) {

  #######################################
  ###### Read in Required Inputs ########
  #######################################


  #
  frac_fullirrig <- calcOutput("RiverSurplusDischargeAllocation", aggregate=FALSE, output="frac_fullirrig", selectyears=selectyears, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, protect_scen=protect_scen)

  # Discharge
  discharge <- calcOutput("RiverSurplusDischargeAllocation", aggregate=FALSE, output="discharge", selectyears=selectyears, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, protect_scen=protect_scen)

  NAg    <- calcOutput("calcRiverHumanUses", aggregate=FALSE, selectyears=selectyears, humanuse="non_agriculture", iniyear=iniyear, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  NAg_ww <- collapseNames(NAg[,,"withdrawal"])
  NAg_wc <- collapseNames(NAg[,,"consumption"])

  CAg    <- calcOutput("calcRiverHumanUses", aggregate=FALSE, selectyears=selectyears, humanuse="committed_agriculture", iniyear=iniyear, climatetype=climatetype, time=time, averaging_range=averaging_range, dof=dof, harmonize_baseline=harmonize_baseline, ref_year=ref_year)
  CAg_ww <- collapseNames(CAg[,,"withdrawal"])
  CAg_wc <- collapseNames(CAg[,,"consumption"])

  #######################
  ###### Outputs ########
  #######################

  #### Return relevant outputs
  out <- mbind(frac_fullirrig, discharge, NAg_ww, NAg_wc, CAg_ww, CAg_wc)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="Cellular discharge after accounting for environmental flow requirements and known human uses along the river",
    isocountries=FALSE))
}
