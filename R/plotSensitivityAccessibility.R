#' @title       plotSensitivityAccessibility
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
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or
#'                         calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                         smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                         smoothed_calib (smoothed, not harmonized, calibrated for proxycrops)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops,
#'                         "meancroprank": rank over mean of proxy crops (normalized),
#'                         "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                         "watervalue": rank over value of irrigation water;
#'                         and fullpotentail TRUE/FALSE separated by ":"
#'                         (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                         FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization", "upstreamfirst")
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
#' plotSensitivityAccessibility(x_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes ggtitle xlab ylab theme_bw theme element_text scale_color_brewer scale_x_continuous scale_y_continuous
#' @importFrom dplyr select matches
#'
#' @export

plotSensitivityAccessibility <- function(x_axis_range, region = "GLO", output, scenario, lpjml, selectyears, climatetype, EFRmethod, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, avlland_scen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  Q100  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "Q:1", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Q100$Accessibility <- rep("Q100", length(Q100$GT))

  Q075  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "Q:0.75", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Q075$Accessibility <- rep("Q075", length(Q075$GT))

  Q050  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "Q:0.5", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Q050$Accessibility <- rep("Q050", length(Q050$GT))

  Q025  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "Q:0.25", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Q025$Accessibility <- rep("Q025", length(Q025$GT))

  Q000  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "Q:0", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Q000$Accessibility <- rep("Q000", length(Q000$GT))

  CV2   <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    EFRmethod = EFRmethod, accessibilityrule = "CV:2", rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, avlland_scen = avlland_scen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  CV2$Accessibility <- rep("CV2", length(CV2$GT))

  # Dummy assignment
  GT            <- "gainthreshold as part of data frame df"
  Accessibility <- "accessibility as part of data frame df"
  Value         <- "Value as part of data frame df"

  df <- rbind.data.frame(Q000, Q025, Q050, CV2, Q075, Q100)
  df <- select(df, GT, Accessibility, matches(paste("IrrigArea", "on", scenario, sep = ".")))
  names(df) <- c("GT", "Accessibility", "Value")

  out <- ggplot(data = df, aes(x = GT, y = Value, color = Accessibility)) +
    geom_line(size = 1.5) + geom_point(aes(y = Value), size = 2) +
    theme_bw() +
    theme(legend.position = c(0.95, 0.9), text = element_text(size = 20)) +
    scale_x_continuous(expand = c(0, 0), breaks = df$GT) + scale_y_continuous(breaks = seq(0, 1000, by = 100), expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste0("Irrigatable Area for yieldcalib = ", yieldcalib, " on ", avlland_scen)) + xlab("Irrigation Costs (USD/ha)") + ylab("Irrigatable Area (Mha)")

  return(out)
}
