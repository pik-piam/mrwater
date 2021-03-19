#' @title       calcWaterPotUse
#' @description This function returns the potential water quantity available for different uses
#'
#' @param selectyears     Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype     Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop        historical crop mix pattern ("historical") or list of proxycrop(s)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{ calcOutput("WaterPotUse", aggregate = FALSE) }
#'

calcWaterPotUse <- function(selectyears, climatetype, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, protect_scen, proxycrop) {

  # Water potentially available for irrigation (accounting for previously committed agricultural uses)
  frac_fullirrig         <- collapseNames(calcOutput("RiverSurplusDischargeAllocation", output="frac_fullirrig", selectyears=selectyears, climatetype=climatetype, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, protect_scen=protect_scen, proxycrop=proxycrop, aggregate=FALSE))
  required_wat_fullirrig <- calcOutput("FullIrrigationRequirement", selectyears=selectyears, climatetype=climatetype, iniyear=iniyear, iniareayear=iniyear, irrigationsystem=irrigationsystem, protect_scen=protect_scen, proxycrop=proxycrop, aggregate=FALSE)
  wat_avl_agr_ww         <- frac_fullirrig * pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]),  0)
  wat_avl_agr_wc         <- frac_fullirrig * pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Water already committed to irrigation
  currHuman    <- calcOutput("RiverHumanUses", humanuse="committed_agriculture", climatetype=climatetype, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
  currHuman_ww <- collapseNames(currHuman[,,"currHuman_ww"])
  currHuman_wc <- collapseNames(currHuman[,,"currHuman_wc"])

  water_ag_ww <- currHuman_ww + wat_avl_agr_ww
  water_ag_ww <- add_dimension(water_ag_ww, dim=3.3, add="wat_pot", nm="wat_ag_ww")
  water_ag_wc <- currHuman_wc + wat_avl_agr_wc
  water_ag_wc <- add_dimension(water_ag_wc, dim=3.3, add="wat_pot", nm="wat_ag_wc")

  out <- mbind(water_ag_ww, water_ag_wc)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="potential water availability for agricultural use",
    isocountries=FALSE))
}
