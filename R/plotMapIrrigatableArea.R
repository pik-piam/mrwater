#' @title       plotMapIrrigatableArea
#' @description map of areas that can be potentially irrigated
#'
#' @param scenario         Non-agricultural water use scenario and EFP "on" or "off" separated by .
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx, e.g. Q75: 0.25, Q50: 0.5)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param gainthreshold    Threshold of yield improvement potential required for water allocation in upstreamfirst algorithm (in tons per ha)
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen  Land availability scenario (currCropland, currIrrig, potIrrig)
#'                      combination of land availability scenario and initialization year separated by ":".
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMapIrrigatableArea()
#' }
#'
#' @importFrom luplot plotmap2
#'
#' @export

plotMapIrrigatableArea <- function(scenario, lpjml, climatetype, selectyears, rankmethod, cropmix, yieldcalib, EFRmethod, accessibilityrule, allocationrule, thresholdtype, gainthreshold, irrigationsystem, avlland_scen, potential_wat, com_ag, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for the map")
  }

  irrigarea  <- collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                         EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
                                         allocationrule = allocationrule, thresholdtype = thresholdtype, gainthreshold = gainthreshold, irrigationsystem = irrigationsystem,
                                         avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = potential_wat, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
  irrigarea  <- dimSums(irrigarea, dim = "season")
  irrigarea[irrigarea == 0] <- NA

  out <- plotmap2(toolLPJcell2MAgPIEcell(irrigarea[, selectyears, scenario]), title = element_blank(), labs = FALSE, sea = FALSE, land_colour = "transparent") +
    scale_fill_continuous("", limit = c(0, 0.25), low = "#ffffe5", high = "darkgreen", na.value = "white") +
    theme(title = element_blank(),
      legend.position = c(0.06, 0.3), legend.direction = "vertical",
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

  return(out)
}
