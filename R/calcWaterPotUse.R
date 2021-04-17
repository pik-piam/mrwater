#' @title       calcWaterPotUse
#' @description This function returns the potential water quantity available for different uses
#'
#' @param selectyears     Years to be returned (Note: does not affect years of harmonization or smoothing)
#' @param climatetype     Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param conservationstatus Conservation status or management objective according to Smakthin EFR method: "fair", "good", "natural". Details: The strictness of the conservation status affects the LFRs (low flow requirements, baseflow that needs to be maintained in the river)
#' @param variabilitythreshold Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod      method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype   Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold   Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          Initialization year of irrigation system
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
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

calcWaterPotUse <- function(selectyears, climatetype, conservationstatus, variabilitythreshold, rankmethod, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, avlland_scen, proxycrop) {

  # Check
  if (!is.na(as.list(strsplit(avlland_scen, split=":"))[[1]][2]) && iniyear != as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])) stop("Initialization year in calcWaterPotUse does not match: iniyear and avlland_scen should have same initialization year")

  # Water potentially available for irrigation (accounting for previously committed agricultural uses)
  frac_fullirrig         <- collapseNames(calcOutput("RiverSurplusDischargeAllocation", output="frac_fullirrig", selectyears=selectyears, climatetype=climatetype, conservationstatus=conservationstatus, variabilitythreshold=variabilitythreshold, rankmethod=rankmethod, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE))
  required_wat_fullirrig <- calcOutput("FullIrrigationRequirement", selectyears=selectyears, climatetype=climatetype, comagyear=iniyear, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)
  wat_avl_agr_ww         <- frac_fullirrig * pmax(collapseNames(required_wat_fullirrig[,,"withdrawal"]),  0)
  wat_avl_agr_wc         <- frac_fullirrig * pmax(collapseNames(required_wat_fullirrig[,,"consumption"]), 0)

  # Water already committed to irrigation
  currHuman    <- calcOutput("RiverHumanUses", humanuse="committed_agriculture", climatetype=climatetype, conservationstatus=conservationstatus, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
  currHuman_ww <- collapseNames(currHuman[,,"currHuman_ww"])
  currHuman_wc <- collapseNames(currHuman[,,"currHuman_wc"])

  # Water use for non-agricultural purposes
  non_ag <- calcOutput("RiverHumanUses", selectyears=selectyears, humanuse="non_agriculture", iniyear=iniyear, climatetype=climatetype, conservationstatus=conservationstatus, aggregate=FALSE)
  non_ag_ww <- collapseNames(non_ag[,,"currHuman_ww"])
  non_ag_wc <- collapseNames(non_ag[,,"currHuman_wc"])

  # Function outputs
  water_ag_ww  <- currHuman_ww + wat_avl_agr_ww
  water_ag_wc  <- currHuman_wc + wat_avl_agr_wc
  water_tot_ww <- water_ag_ww + non_ag_ww
  water_tot_wc <- water_ag_wc + non_ag_wc

  water_ag_ww  <- add_dimension(water_ag_ww, dim=3.3, add="wat_pot", nm="wat_ag_ww")
  water_ag_wc  <- add_dimension(water_ag_wc, dim=3.3, add="wat_pot", nm="wat_ag_wc")
  water_tot_ww <- add_dimension(water_tot_ww, dim=3.3, add="wat_pot", nm="water_tot_ww")
  water_tot_wc <- add_dimension(water_tot_wc, dim=3.3, add="wat_pot", nm="water_tot_wc")

  out <- mbind(water_ag_ww, water_ag_wc, water_tot_ww, water_tot_wc)

  return(list(
    x=out,
    weight=NULL,
    unit="mio. m^3",
    description="potential water availability for agricultural use or total human water use",
    isocountries=FALSE))
}
