#' @title       plotSensitivityAllocationrule
#' @description plot of irrigatable area depending on costs paid for irrigation for different water accessibility
#'
#' @param region           regional resolution (can be country iso-code, "GLO" for global,
#'                         region name and respective mapping "EUR:H12")
#' @param output           output to be displayed: irrigated area "IrrigArea" or
#'                         available water volume "wat_ag_ww" "wat_ag_wc"
#' @param x_axis_range     range of x-axis (gainthreshold) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or
#'                         calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                         smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                         smoothed_calib (smoothed, not harmonized, calibrated for proxycrops)
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm:
#'                         TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param avlland_scen     Land availability scenario: current or potential;
#'                         optional additionally: protection scenario in case of potential (when left empty: no protection) and initialization year of cropland area
#'                         combination of land availability scenario and initialization year separated by ":". land availability scenario: currIrrig (only currently irrigated cropland available for irrigated agriculture), currCropland (only current cropland areas available for irrigated agriculture), potIrrig (suitable land is available for irrigated agriculture, potentially land restrictions activated through protect_scen argument)
#'                         protection scenario separated by "_" (only relevant when potIrrig selected):
#'                         WDPA, BH, FF, CPD, LW, HalfEarth
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation,
#'                         FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotSensitivityAllocationrule(x_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes ggtitle xlab ylab theme_bw theme element_text scale_color_brewer scale_x_continuous scale_y_continuous
#' @importFrom dplyr select matches
#'
#' @export

plotSensitivityAllocationrule <- function(x_axis_range, region = "GLO", output, scenario, lpjml, selectyears, climatetype, EFRmethod, accessibilityrule, yieldcalib, thresholdtype, irrigationsystem, avlland_scen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  Opt  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:TRUE", yieldcalib = yieldcalib,
    allocationrule = "optimization", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Opt$Allocationrule <- rep("Opt", length(Opt$GT))

  OptRed  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:FALSE", yieldcalib = yieldcalib,
                            allocationrule = "optimization", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                            cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  OptRed$Allocationrule <- rep("OptRed", length(OptRed$GT))

  Up  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                            EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:TRUE", yieldcalib = yieldcalib,
                            allocationrule = "upstreamfirst", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
                            cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Up$Allocationrule <- rep("Up", length(Up$GT))

  # Dummy assignment
  GT             <- "gainthreshold as part of data frame df"
  Allocationrule <- "accessibility as part of data frame df"
  Value          <- "Value as part of data frame df"

  df <- rbind.data.frame(Opt, OptRed, Up)
  df <- select(df, GT, Allocationrule, matches(paste("IrrigArea", "on", scenario, sep = ".")))
  names(df) <- c("GT", "Allocationrule", "Value")

  out <- ggplot(data = df, aes(x = GT, y = Value, color = Allocationrule)) +
    geom_line(size = 1.5) + geom_point(aes(y = Value), size = 2) +
    theme_bw() +
    theme(legend.position = c(0.95, 0.9), text = element_text(size = 20)) +
    scale_x_continuous(expand = c(0, 0), breaks = df$GT) + scale_y_continuous(breaks = seq(0, 1000, by = 100), expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste0("Irrigatable Area for yieldcalib = ", yieldcalib, " on ", avlland_scen)) + xlab("Irrigation Costs (USD/ha)") + ylab("Irrigatable Area (Mha)")

  return(out)
}
