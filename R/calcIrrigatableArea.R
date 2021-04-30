#' @title       calcIrrigatableArea
#' @description calculates area that can potentially be irrigated given available water and land
#'
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param variabilitythreshold Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param FAOyieldcalib    TRUE (LPJmL yields scaled with current FAO yield) or FALSE (LPJmL yield potentials)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigatableArea", aggregate=FALSE) }
#'
#' @import magclass
#' @import magpiesets

calcIrrigatableArea <- function(lpjml, selectyears, climatetype, EFRmethod, variabilitythreshold, rankmethod, FAOyieldcalib, allocationrule, thresholdtype, gainthreshold, irrigationsystem, avlland_scen, proxycrop, potential_wat){

  # retrieve function arguments
  iniyear <- as.numeric(as.list(strsplit(avlland_scen, split=":"))[[1]][2])

  ## Read in water available for irrigation
  if (potential_wat) {
    wat_avl         <- calcOutput("WaterPotUse", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod, variabilitythreshold=variabilitythreshold, rankmethod=rankmethod, FAOyieldcalib=FAOyieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, avlland_scen=avlland_scen, proxycrop=proxycrop, aggregate=FALSE)
    wat_avl_irrig_c <- collapseNames(wat_avl[,,"wat_ag_wc"])
    wat_avl_irrig_w <- collapseNames(wat_avl[,,"wat_ag_ww"])
  } else {
    wat_avl         <- calcOutput("RiverHumanUses", lpjml=lpjml, selectyears=selectyears, humanuse="committed_agriculture", iniyear=iniyear, climatetype=climatetype, EFRmethod=EFRmethod, aggregate=FALSE)
    wat_avl_irrig_c <- collapseNames(wat_avl[,,"currHuman_wc"])
    wat_avl_irrig_w <- collapseNames(wat_avl[,,"currHuman_ww"])
  }

  # Irrigation water requirements for selected cropmix and irrigation system per cell (in mio. m^3)
  wat_req    <- calcOutput("FullIrrigationRequirement", lpjml=lpjml, climatetype=climatetype, selectyears=selectyears, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, comagyear=NULL, aggregate=FALSE)
  wat_req_ww <- collapseNames(wat_req[,,"withdrawal"])
  wat_req_wc <- collapseNames(wat_req[,,"consumption"])

  ## Read in area that can potentially be irrigated (including total potentially irrigatable area; defined by comagyear=NULL)
  area_potirrig <- calcOutput("AreaPotIrrig", selectyears=selectyears, avlland_scen=avlland_scen, comagyear=NULL, aggregate=FALSE)

  # share of requirements that can be fulfilled given available water, when >1 whole area can be irrigated
  irrigarea_ww <- pmin(wat_avl_irrig_w / wat_req_ww, 1) * area_potirrig
  irrigarea_ww[wat_req_ww==0] <- 0      # cells with no water requirements also get no irrigated area assigned
  irrigarea_ww <- add_dimension(irrigarea_ww, dim=3.3, add="data", nm="irrigatable_ww")

  irrigarea_wc <- pmin(wat_avl_irrig_c / wat_req_wc, 1) * area_potirrig
  irrigarea_wc[wat_req_wc==0] <- 0
  irrigarea_wc <- add_dimension(irrigarea_wc, dim=3.3, add="data", nm="irrigatable_wc")

  irrigatable_area <- pmin(collapseNames(irrigarea_ww), collapseNames(irrigarea_wc))
  irrigatable_area <- add_dimension(irrigatable_area, dim=3.3, add="data", nm="irrigatable")

  out <- mbind(irrigatable_area, irrigarea_ww, irrigarea_wc)

  # check for NAs and negative values
  if (any(is.na(out))) {
    stop("produced NA irrigatable area")
  }
  if (any(out<0)) {
    stop("produced negative irrigatable")
  }

  return(list(
    x=out,
    weight=NULL,
    unit="mio. ha",
    description="Area that can be irrigated given land and water constraints",
    isocountries=FALSE))
}
