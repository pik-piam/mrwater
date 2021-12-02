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
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction:
#'                          discharge that is exceeded x percent of the time on average throughout a year (Qx).
#'                          Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param thresholdtype     Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm:
#'                          TRUE: monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem  Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. available land scenario (currCropland, currIrrig, potCropland)
#'                          2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of no land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Selected cropmix (options:
#'                          "hist_irrig" for historical cropmix on currently irrigated area,
#'                          "hist_total" for historical cropmix on total cropland,
#'                          or selection of proxycrops)
#' @param potential_wat     if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag            if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation,
#'                          FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
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

plotSensitivityAllocationrule <- function(x_axis_range, region = "GLO", output, scenario, lpjml, selectyears, iniyear, climatetype, efrMethod, accessibilityrule, yieldcalib, thresholdtype, irrigationsystem, landScen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  Opt  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml,
                            selectyears = selectyears, iniyear = iniyear, climatetype = climatetype,
    efrMethod = efrMethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:TRUE", yieldcalib = yieldcalib,
    allocationrule = "optimization", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
    cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  Opt$Allocationrule <- rep("Opt", length(Opt$GT))

  OptRed  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, iniyear = iniyear, climatetype = climatetype,
                            efrMethod = efrMethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:FALSE", yieldcalib = yieldcalib,
                            allocationrule = "optimization", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
                            cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)$data
  OptRed$Allocationrule <- rep("OptRed", length(OptRed$GT))

  Up  <- reportEconOfIrrig(GT_range = x_axis_range, region = region, output = output, scenario = scenario, lpjml = lpjml, selectyears = selectyears, iniyear = iniyear, climatetype = climatetype,
                            efrMethod = efrMethod, accessibilityrule = accessibilityrule, rankmethod = "meanpricedcroprank:TRUE", yieldcalib = yieldcalib,
                            allocationrule = "upstreamfirst", thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen,
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
    ggtitle(paste0("Irrigatable Area for yieldcalib = ", yieldcalib, " on ", landScen)) + xlab("Irrigation Costs (USD/ha)") + ylab("Irrigatable Area (Mha)")

  return(out)
}
