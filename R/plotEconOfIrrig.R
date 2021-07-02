#' @title       plotEconOfIrrig
#' @description plot of irrigatable area depending on costs paid for irrigation
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param output           output to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc"
#' @param x_axis_range     range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
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
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotEconOfIrrig(x_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes_string ggtitle xlab ylab theme_bw
#'
#' @export

plotEconOfIrrig <- function(region = "GLO", x_axis_range, output, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  inputdata   <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype, EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen, cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)
  df          <- inputdata$data
  description <- inputdata$description
  unit        <- inputdata$unit

  out <- ggplot(data = df, aes_string(x = "GT")) +
    geom_line(aes_string(y = paste("IrrigArea", "on", scenario, sep = ".")),  color = "darkblue")                    + geom_point(aes_string(y = paste("IrrigArea", "on", scenario, sep = "."))) +
    geom_line(aes_string(y = paste("IrrigArea", "off", scenario, sep = ".")), color = "darkred", linetype = "twodash") + geom_point(aes_string(y = paste("IrrigArea", "off", scenario, sep = "."))) +
    theme_bw() +
    ggtitle(paste0(description, " for yieldcalib = ", yieldcalib, " on ", avlland_scen)) + xlab("Irrigation Costs (USD/ha)") + ylab(unit)

  return(out)
}
