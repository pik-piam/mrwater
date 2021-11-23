#' @title       plotMarginalReturnToIrrigation
#' @description plot minimum monetary yield gain achieved on irrigated area
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param y_axis_range     range of y-axis (monetary irrigation gain) to be depicted on the curve
#' @param x_axis           x_axis type to be displayed: irrigated area "IrrigArea" or available water volume "wat_ag_ww" "wat_ag_wc" "wat_tot_ww" "wat_tot_wc"
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib        If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                          If FALSE: uncalibrated LPJmL yields are used
#' @param rankmethod        method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule    Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype     Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem  Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param landScen          Land availability scenario consisting of two parts separated by ":":
#'                          1. landScen (currCropland, currIrrig, potCropland)
#'                          2. for curr-scenarios: initialization year;
#'                          for pot-scenarios: protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_FF, NA).
#'                          For case of pot-scenario without land protection select "NA"
#'                          or do not specify second part of the argument
#' @param cropmix           Cropmix for which irrigation yield improvement is calculated
#'                          can be selection of proxycrop(s) for calculation of average yield gain
#'                          or hist_irrig or hist_total for historical cropmix
#' @param potential_wat     if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag            if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotMarginalReturnToIrrigation(y_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom ggplot2 ggplot geom_line geom_vline geom_point aes_string ggtitle xlab ylab theme_bw
#'
#' @export

plotMarginalReturnToIrrigation <- function(y_axis_range, x_axis, region = "GLO", scenario, lpjml, selectyears, iniyear, climatetype, efrMethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, landScen, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  # Main data
  inputdata   <- reportEconOfIrrig(GT_range = y_axis_range, region = region, output = x_axis, scenario = scenario,
    lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
    efrMethod = efrMethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = yieldcalib,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
    landScen = landScen, cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)
  tmp         <- inputdata$data
  names(tmp)[-1] <- paste(names(tmp)[-1], "LPJmL", sep = ".")
  inputdata   <- reportEconOfIrrig(GT_range = y_axis_range, region = region, output = x_axis, scenario = scenario, lpjml = lpjml, selectyears = selectyears, climatetype = climatetype, efrMethod = efrMethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = TRUE,
    allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem, landScen = landScen, cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)
  df          <- inputdata$data
  names(df)[-1] <- paste(names(df)[-1], "FAO", sep = ".")
  df          <- merge(df, tmp)
  description <- inputdata$description
  unit        <- inputdata$unit

  # Reference lines
  if (x_axis == "IrrigArea") {
    # Area that can be irrigated with committed agricultural uses
    current_fulfilled <- collapseNames(calcOutput("IrrigatableArea",
                                                  gainthreshold = 0, selectyears = selectyears, iniyear = iniyear,
                                                  climatetype = climatetype, lpjml = lpjml,
                                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod, rankmethod = rankmethod,
                                                  yieldcalib = yieldcalib, allocationrule = allocationrule, thresholdtype = thresholdtype,
                                                  irrigationsystem = irrigationsystem, landScen = landScen, cropmix = cropmix,
                                                  potential_wat = FALSE, com_ag = com_ag, multicropping = multicropping, aggregate = FALSE)[, , "irrigatable"])
    current_fulfilled <- dimSums(current_fulfilled, dim = "season")
    current_LUH <- dimSums(calcOutput("CropareaAdjusted", years = iniyear,
                                      sectoral = "kcr", cells = "lpjcell", physical = TRUE,
                                      cellular = TRUE, irrigation = TRUE, aggregate = FALSE)[, , "irrigated"], dim = 3)

  } else {
    # Water already committed to irrigation
    tmp    <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                         lpjml = lpjml, climatetype = climatetype, efrMethod = efrMethod, selectyears = selectyears,
                         iniyear = iniyear, aggregate = FALSE)
    current_fulfilled <- collapseNames(tmp[, , x_axis])
  }

  # sum up over regional dimension
  current_fulfilled <- toolRegionSums(x = current_fulfilled, region = region)
  current_LUH       <- toolRegionSums(x = current_LUH, region = region)

  out <- ggplot(data = df, aes_string(y = "GT")) +
    geom_line(aes_string(x = paste(x_axis, "on", scenario, "LPJmL", sep = ".")),  color = "darkblue", linetype = "dotted", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "on", scenario, "LPJmL", sep = "."))) +
    geom_line(aes_string(x = paste(x_axis, "off", scenario, "LPJmL", sep = ".")), color = "darkred", linetype = "dotted", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "off", scenario, "LPJmL", sep = "."))) +
    geom_line(aes_string(x = paste(x_axis, "on", scenario, "FAO", sep = ".")),  color = "darkblue", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "on", scenario, "FAO", sep = "."))) +
    geom_line(aes_string(x = paste(x_axis, "off", scenario, "FAO", sep = ".")), color = "darkred", size = 1.1)  + geom_point(aes_string(x = paste(x_axis, "off", scenario, "FAO", sep = "."))) +
    theme_bw() +
    geom_vline(xintercept = as.numeric(current_fulfilled[, , paste("on", scenario, sep = ".")]), color = "blue", size = 1.01) +
    geom_vline(xintercept = as.numeric(current_fulfilled[, , paste("off", scenario, sep = ".")]), color = "red", size = 1.01) +
    geom_vline(xintercept = as.numeric(current_LUH), color = "black", linetype = "dashed", size = 1.01) +
    ggtitle(paste0("Marginal return to ", description, " on ", landScen)) + ylab("Monetary yield gain (USD/ha)") + xlab(unit)

  return(out)
}
