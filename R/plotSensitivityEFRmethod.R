#' @title       plotSensitivityEFRmethod
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
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops,
#'                         "meancroprank": rank over mean of proxy crops (normalized),
#'                         "meanpricedcroprank": rank over mean of proxy crops (normalized using price),
#'                         "watervalue": rank over value of irrigation water;
#'                         and fullpotentail TRUE/FALSE separated by ":"
#'                         (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area).
#'                         FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization", "upstreamfirst")
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm:
#'                         TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. landScen (currCropland, currIrrig, potCropland)
#'                          2. for curr-scenarios: initialization year;
#'                          for pot-scenarios: protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of pot-scenario without land protection select "NA"
#'                          or do not specify second part of the argument
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
#' plotSensitivityEFRmethod(x_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom ggplot2 ggplot geom_line geom_point aes ggtitle xlab ylab theme_bw theme element_text scale_color_brewer scale_x_continuous scale_y_continuous
#' @importFrom dplyr select matches
#'
#' @export

plotSensitivityEFRmethod <- function(x_axis_range, region = "GLO", output, scenario, lpjml, selectyears, climatetype, allocationrule, accessibilityrule, rankmethod, yieldcalib, thresholdtype, irrigationsystem, landScen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  VMF  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    efrMethod = "VMF:fair", accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  VMF$EFR <- rep("VMF", length(VMF$GT))

  Sfair  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    efrMethod = "Smakhtin:fair", accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Sfair$EFR <- rep("Sfair", length(Sfair$GT))

  Sgood  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    efrMethod = "Smakhtin:good", accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Sgood$EFR <- rep("Sgood", length(Sgood$GT))

  Snatural  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    efrMethod = "Smakhtin:natural", accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Snatural$EFR <- rep("Snatural", length(Snatural$GT))

  # Dummy assignment
  GT    <- "gainthreshold as part of data frame df"
  EFR   <- "EFR as part of data frame df"
  Value <- "Value as part of data frame df"

  df <- rbind.data.frame(VMF, Sfair, Sgood, Snatural)
  df <- select(df, GT, EFR, matches(paste("IrrigArea", "on", scenario, sep = ".")))
  names(df) <- c("GT", "EFR", "Value")

  out <- ggplot(data = df, aes(x = GT, y = Value, color = EFR)) +
    geom_line(size = 1.5) + geom_point(aes(y = Value), size = 2) +
    theme_bw() +
    theme(legend.position = c(0.95, 0.9), text = element_text(size = 20)) +
    scale_x_continuous(expand = c(0, 0), breaks = df$GT) + scale_y_continuous(breaks = seq(0, 1000, by = 100), expand = c(0, 0)) +
    scale_color_brewer(palette = "Dark2") +
    ggtitle(paste0("Irrigatable Area for yieldcalib = ", yieldcalib, " on ", landScen)) + xlab("Irrigation Costs (USD/ha)") + ylab("Irrigatable Area (Mha)")

  return(out)
}
