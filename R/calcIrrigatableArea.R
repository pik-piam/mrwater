#' @title       calcIrrigatableArea
#' @description calculates area that can potentially be irrigated given available water and land
#'
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param iniyear          year of initialization for cropland area initialization and irrigation systems
#' @param protect_scen     land protection scenario: NULL (no irrigation limitation in protected areas), WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ calcOutput("IrrigatableArea", aggregate=FALSE) }
#'
#' @import magclass
#' @import magpiesets

calcIrrigatableArea <- function(selectyears, climatetype, allocationrule, thresholdtype, gainthreshold, irrigationsystem, iniyear, protect_scen, proxycrop){

  ## Read in water available for irrigation
  wat_avl <- calcOutput("WaterPotUse", selectyears=selectyears, climatetype=climatetype, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, iniyear=iniyear, protect_scen=protect_scen, proxycrop=proxycrop, aggregate=FALSE)
  wat_avl_irrig_c <- wat_avl[,,"wat_ag_wc"]
  wat_avl_irrig_w <- wat_avl[,,"wat_ag_ww"]

  wat_req    <- calcOutput("FullIrrigationRequirement", climatetype=climatetype, selectyears=selectyears, iniyear=iniyear, irrigationsystem=irrigationsystem, protect_scen=protect_scen, proxycrop=proxycrop, iniareayear=NULL, aggregate=FALSE)
  wat_req_ww <- collapseNames(wat_req[,,"withdrawal"])
  wat_req_wc <- collapseNames(wat_req[,,"consumption"])

  ## Read in area that can potentially be irrigated (including total potentially irrigatable area; defined by iniareayear=NULL)
  area_potirrig <- calcOutput("AreaPotIrrig", selectyears=selectyears, protect_scen=protect_scen, iniareayear=NULL, aggregate=FALSE)

  irrigarea_ww <- pmin(wat_avl_irrig_w / wat_req_ww, 1) * area_potirrig
  irrigarea_ww[wat_req_ww==0] <- 0
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
