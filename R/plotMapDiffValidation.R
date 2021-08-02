#' @title       plotMapDiffValidation
#' @description map of difference between potential irrigation and actual irrigation
#'
#' @param reference        LUH or committed
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
#' @param avlland_scen  Land availability scenario (currCropland, currIrrig, potIrrig)
#'                      combination of land availability scenario and initialization year separated by ":".
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
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

plotMapDiffValidation <- function(reference, scenario, lpjml, climatetype, selectyears, gainthreshold, rankmethod, cropmix, yieldcalib, EFRmethod, accessibilityrule, allocationrule, thresholdtype, irrigationsystem, avlland_scen, multicropping) {

  if (length(selectyears) > 1) {
    stop("Please select one year only for the map")
  }

  x1000 <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = gainthreshold,
    selectyears = selectyears, climatetype = climatetype, accessibilityrule = accessibilityrule,
    EFRmethod = EFRmethod, rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
    thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = FALSE, multicropping = multicropping, aggregate = FALSE)[, , paste(scenario, "irrigatable", sep = ".")]), dim = "season")
  if (reference == "committed") {
    xC  <- dimSums(collapseNames(calcOutput("IrrigatableArea", lpjml = lpjml, gainthreshold = 0, selectyears = selectyears,
      climatetype = climatetype, accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule, thresholdtype = thresholdtype,
      irrigationsystem = irrigationsystem, avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = FALSE,
      com_ag = TRUE, multicropping = multicropping, aggregate = FALSE)[, , paste(scenario, "irrigatable", sep = ".")]), dim = "season")
  } else if (reference == "LUH") {
    xC <- dimSums(calcOutput("Croparea", years = selectyears, sectoral = "kcr", cells = "lpjcell",
      physical = TRUE, cellular = TRUE, irrigation = TRUE, aggregate = FALSE)[, , "irrigated"], dim = 3)
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
    map                    <- toolGetMappingCoord2Country()
    getCells(xC)           <- paste(map$coords, map$iso, sep = ".")
    names(dimnames(xC))[1] <- "x.y.iso"
    #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  } else {
    stop("Please choose the reference scenario for difference map: committed or LUH")
  }

  diff          <- x1000 - xC
  diff[xC == 0] <- NA

  out <- plotmap2(toolLPJcell2MAgPIEcell(diff[, , "off"]), midcol = "white", midpoint = 0, highcol = "darkblue", lowcol = "darkred", legendname = "Mha") +
    theme(title = element_blank(),
      panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA),
      panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
      strip.background = element_rect(fill = "transparent", colour = NA), strip.text = element_text(color = "white"))

  return(out)
}
