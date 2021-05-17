#' @title       plotShrCurrIrrigFullfilled
#' @description plot map of share of current irrigation that can be fulfilled given surface water availability of the algorithm
#'
#' @param scenario         EFP and non-agricultural water use scenario separated by "." (e.g. "on.ssp2")
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#'
#' @return map of magpie cells
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{ plotShrCurrIrrigFullfilled() }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotShrCurrIrrigFullfilled <- function(scenario, iniyear, lpjml, selectyears, climatetype, EFRmethod) {

  ### Reasons for not-fulfilled actually observed irrigation:
  # - fossil groundwater is used for irrigation (e.g. Northern India), but not accounted for in the river routing
  # - long-distance water diversions take place (e.g. Northern China), but not accounted for in the river routing
  # - deficit irrigation is in place (e.g. Southern Spain), but not accounted for in the river routing
  # - water reuse is not accounted for in the river routing

  if (length(selectyears)>1) {
    stop("Please select one year only for Map depicting the share of current irrigation that can be fulfilled given surface water availability of the algorithm")
  }

  # Committed Agricultural Water (in mio. m^3)
  CAU_magpie <- calcOutput("WaterUseCommittedAg", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, iniyear=iniyear, aggregate=FALSE)
  act_ww <- collapseNames(dimSums(CAU_magpie[,,"withdrawal"], dim=3))
  act_wc <- collapseNames(dimSums(CAU_magpie[,,"consumption"], dim=3))

  # Water Committed to Agriculture after Routing (in mio. m^3)
  ComAg_wat <- calcOutput("RiverHumanUses", humanuse="committed_agriculture", lpjml=lpjml, climatetype=climatetype, EFRmethod=EFRmethod, selectyears=selectyears, iniyear=iniyear, aggregate=FALSE)
  com_ww <- collapseNames(ComAg_wat[,,"currHuman_ww"])
  com_wc <- collapseNames(ComAg_wat[,,"currHuman_wc"])

  ww_shr <- com_ww / act_ww
  ww_shr[act_ww==0 & com_ww==0] <- 0
  ww_shr[act_ww==0] <- NA
  wc_shr <- com_wc / act_wc
  wc_shr[act_wc==0 & com_wc==0] <- 0
  wc_shr[act_wc==0] <- NA

  out <- plotmap2(toolLPJarrayToMAgPIEmap(ww_shr[,selectyears,scenario]), title=paste("Share of current water withdrawal for irrigated agriculture that can be fulfilled by available surface water"), legendname = "%", text_size = 6)

  return(out)
}
