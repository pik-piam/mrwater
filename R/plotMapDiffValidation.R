#' @title       plotMapDiffValidation
#' @description map of difference between potential irrigation and actual irrigation
#'
#' @param scenario         Non-agricultural water use scenario
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential; optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected): WDPA, BH, FF, CPD, LW, HalfEarth. Areas where no irrigation water withdrawals are allowed due to biodiversity protection.
#' @param proxycrop        proxycrop(s) selected for crop mix specific calculations: average over proxycrop(s) yield gain. NULL returns all crops individually
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMapDiffValidation()
#' }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotMapDiffValidation <- function(scenario, lpjml, climatetype, selectyears, gainthreshold, rankmethod, proxycrop, yieldcalib, EFRmethod, accessibilityrule, allocationrule, thresholdtype, irrigationsystem, avlland_scen, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for the map")
  }

  x1000 <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
                                    selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
                                    EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                    thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                                    proxycrop = proxycrop, potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE)[, , paste(scenario, "irrigatable", sep = ".")])
  xC    <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 0, selectyears = selectyears,
                                    climatetype = climatetype, accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                    rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule, thresholdtype = thresholdtype,
                                    irrigationsystem = irrigationsystem, avlland_scen = avlland_scen, proxycrop = proxycrop, potential_wat = FALSE,
                                    com_ag = FALSE, multicropping = multicropping, aggregate = FALSE)[, , paste(scenario, "irrigatable", sep = ".")])
  diff <- x1000 - xC
  diff[xC == 0] <- NA

  out <- plotmap2(toolLPJcell2MAgPIEcell(diff[, , "off"]), midcol = "white", midpoint = 0, highcol = "darkblue", lowcol = "darkred", legendname = "Mha") +
    theme(title = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

  # irrigarea_pot  <- calcOutput("IrrigatableArea", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod, accessibilityrule=accessibilityrule, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag="discharge_only", aggregate=FALSE)
  # irrigarea_act  <- calcOutput("IrrigatableArea", lpjml=lpjml, selectyears=selectyears, climatetype=climatetype, EFRmethod=EFRmethod, accessibilityrule=accessibilityrule, rankmethod=rankmethod, yieldcalib=yieldcalib, allocationrule=allocationrule, thresholdtype=thresholdtype, gainthreshold=gainthreshold, irrigationsystem=irrigationsystem, avlland_scen=avlland_scen, proxycrop=proxycrop, potential_wat=TRUE, com_ag=TRUE, aggregate=FALSE)
  #
  # diff <- irrigarea_pot[,,"off"] - irrigarea_act[,,"off"]
  # diff <- collapseNames(diff[,,"irrigatable"])
  #
  # out <- plotmap2(toolLPJcell2MAgPIEcell(diff[,selectyears,scenario]), legendname = "Mha",
  #                 lowcol = "darkred", midcol = "white", highcol = "darkblue", midpoint = 0,
  #                 title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
  #   theme(title=element_blank(),
  #         panel.background = element_rect(fill="transparent", colour=NA),  plot.background = element_rect(fill = "transparent", colour = NA),
  #         panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
  #         strip.background = element_rect(fill="transparent", colour=NA), strip.text = element_text(color="white"))

  return(out)
}