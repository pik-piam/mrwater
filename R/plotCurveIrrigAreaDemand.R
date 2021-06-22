#' @title       plotCurveIrrigAreaDemand
#' @description plot minimum monetary yield gain per ha achieved
#'              on related potentially irrigated area
#'
#' @param region           regional resolution (can be country iso-code, region name and respective mapping "EUR:H12", "GLO" for global)
#' @param y_axis_range     range of y-axis (monetary irrigation gain) to be depicted on the curve
#' @param scenario         non-agricultural water use scenario to be displayed in plot
#' @param lpjml            LPJmL version required for respective inputs: natveg or crop
#' @param selectyears      years for which irrigatable area is calculated
#' @param iniyear          initialization year
#' @param climatetype      Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param EFRmethod        EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param accessibilityrule Scalar value defining the strictness of accessibility restriction: discharge that is exceeded x percent of the time on average throughout a year (Qx). Default: 0.5 (Q50) (e.g. Q75: 0.25, Q50: 0.5)
#' @param yieldcalib       FAO (LPJmL yields calibrated with current FAO yield) or calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or none (smoothed LPJmL yield potentials, not harmonized, not calibrated)
#' @param rankmethod       method of calculating the rank: "meancellrank" (default): mean over cellrank of proxy crops, "meancroprank": rank over mean of proxy crops (normalized), "meanpricedcroprank": rank over mean of proxy crops (normalized using price), "watervalue": rank over value of irrigation water; and fullpotentail TRUE/FALSE separated by ":" (TRUE: Full irrigation potential (cell receives full irrigation requirements in total area). FALSE: reduced potential of cell receives at later stage in allocation algorithm)
#' @param allocationrule   Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param thresholdtype    Thresholdtype of yield improvement potential required for water allocation in upstreamfirst algorithm: TRUE (default): monetary yield gain (USD05/ha), FALSE: yield gain in tDM/ha
#' @param irrigationsystem Irrigation system to be used for river basin discharge allocation algorithm ("surface", "sprinkler", "drip", "initialization")
#' @param cropmix          cropmix for which irrigation yield improvement is calculated
#'                         can be selection of proxycrop(s) for calculation of average yield gain
#'                         or hist_irrig or hist_total for historical cropmix
#' @param potential_wat    if TRUE: potential available water and areas used, if FALSE: currently reserved water on current irrigated cropland used
#' @param com_ag           if TRUE: the currently already irrigated areas in initialization year are reserved for irrigation, if FALSE: no irrigation areas reserved (irrigation potential)
#' @param multicropping    Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' plotCurveIrrigAreaDemand(y_axis_range = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass dimSums collapseNames
#' @importFrom ggplot2 ggplot geom_line geom_vline geom_point aes_string ggtitle xlab ylab theme_bw geom_text
#'
#' @export

plotCurveIrrigAreaDemand <- function(y_axis_range, region = "GLO", scenario, lpjml, selectyears, iniyear, climatetype, EFRmethod, accessibilityrule, rankmethod, yieldcalib, allocationrule, thresholdtype, irrigationsystem, cropmix, potential_wat = TRUE, com_ag, multicropping) {

  ## Main data: with water constraint
  # on current cropland
  inputdata <- reportEconOfIrrig(GT_range = y_axis_range, region = region, output = "IrrigArea", scenario = scenario,
                                  lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                  EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = FALSE,
                                  allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                  avlland_scen = "currCropland:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)
  tmp            <- inputdata$data
  names(tmp)[-1] <- paste(names(tmp)[-1], "CurrCropland", sep = ".")

  # on potential cropland
  inputdata <- reportEconOfIrrig(GT_range = y_axis_range, region = region, output = "IrrigArea", scenario = scenario,
                                lpjml = lpjml, selectyears = selectyears, climatetype = climatetype,
                                EFRmethod = EFRmethod, accessibilityrule = accessibilityrule, rankmethod = rankmethod, yieldcalib = TRUE,
                                allocationrule = allocationrule, thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                avlland_scen = "potIrrig_HalfEarth:2010", cropmix = cropmix, potential_wat = TRUE, com_ag = com_ag, multicropping = multicropping)
  df            <- inputdata$data
  names(df)[-1] <- paste(names(df)[-1], "PotCropland", sep = ".")
  df            <- merge(df, tmp)
  description   <- inputdata$description
  unit          <- inputdata$unit

  # Reference data: without water constraint
  tmp <- reportYieldgainArea(region = region, GT_range = y_axis_range, lpjml = lpjml,
                              selectyears = selectyears, climatetype = climatetype, EFRmethod = EFRmethod,
                              yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "currCropland:2010",
                              cropmix = cropmix, multicropping = multicropping)$data
  names(tmp)[-1] <- paste(names(tmp)[-1], "CurrCropland", sep = ".")
  df   <- merge(df, tmp)
  tmp  <- reportYieldgainArea(region = region, GT_range = y_axis_range, lpjml = lpjml,
                              selectyears = selectyears, climatetype = climatetype, EFRmethod = EFRmethod,
                              yieldcalib = yieldcalib, thresholdtype = thresholdtype, avlland_scen = "potIrrig_HalfEarth:2010",
                              cropmix = cropmix, multicropping = multicropping)$data
  names(tmp)[-1] <- paste(names(tmp)[-1], "PotCropland", sep = ".")
  df <- merge(df, tmp)

  ## Reference points
  # Area that can be irrigated with committed agricultural uses
  current_fulfilled <- collapseNames(calcOutput("IrrigatableArea", gainthreshold = 0,
                                      selectyears = selectyears, climatetype = climatetype, lpjml = lpjml,
                                      accessibilityrule = accessibilityrule, EFRmethod = EFRmethod,
                                      rankmethod = rankmethod, yieldcalib = yieldcalib, allocationrule = allocationrule,
                                      thresholdtype = thresholdtype, irrigationsystem = irrigationsystem,
                                      avlland_scen = "currCropland:2010", cropmix = cropmix, multicropping = multicropping,
                                      potential_wat = FALSE, com_ag = TRUE, aggregate = FALSE)[, , "irrigatable"])
  # Area that is irrigated according to LUH
  current_LUH <- dimSums(calcOutput("Croparea", years = iniyear, sectoral = "kcr",
                                    cells = "lpjcell", physical = TRUE, cellular = TRUE,
                                    irrigation = TRUE, aggregate = FALSE)[, , "irrigated"], dim = 3)
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####
  map                             <- toolGetMappingCoord2Country()
  getCells(current_LUH)           <- paste(map$coords, map$iso, sep = ".")
  names(dimnames(current_LUH))[1] <- "x.y.iso"
  #### adjust cell name (until 67k cell names fully integrated in calcCroparea and calcLUH2v2!!!) ####

  # sum up over regional dimension
  current_fulfilled <- toolRegionSums(x = current_fulfilled, region = region)
  current_LUH       <- toolRegionSums(x = current_LUH,       region = region)

  out <- ggplot(data = df, aes_string(y = "GT")) +
    # geom_line(aes_string(x = paste("Potentially Irrigated Area on", scenario, "CurrCropland", sep = ".")),  color = "darkblue", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "on", scenario, "CurrCropland", sep = "."))) +
    # geom_line(aes_string(x = paste(x_axis, "off", scenario, "CurrCropland", sep = ".")), color = "darkred", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "off", scenario, "CurrCropland", sep = "."))) +
    # geom_line(aes_string(x = paste(x_axis, "on", scenario, "PotCropland", sep = ".")),  color = "darkblue", linetype = "dotted", size = 1.1) + geom_point(aes_string(x = paste(x_axis, "on", scenario, "PotCropland", sep = "."))) +
    # geom_line(aes_string(x = paste(x_axis, "off", scenario, "PotCropland", sep = ".")), color = "darkred", linetype = "dotted", size = 1.1)  + geom_point(aes_string(x = paste(x_axis, "off", scenario, "PotCropland", sep = "."))) +
    theme_bw() +
    # geom_point(x = as.numeric(current_fulfilled[, , paste("on", scenario, sep = ".")]), y = 0, color = "blue", size = 1.1) +
    # geom_text(aes(label=ifelse((x == as.numeric(current_fulfilled[, , paste("on", scenario, sep = ".")]) & y == 0), as.character("EFP on"), "")), hjust = 0, vjust = -0.5)
    # geom_point(x = as.numeric(current_fulfilled[, , paste("on", scenario, sep = ".")]), y = 0, color = "red", size = 1.1) +
    # geom_text(aes(label=ifelse((x == as.numeric(current_fulfilled[, , paste("off", scenario, sep = ".")]) & y == 0), as.character("EFP off"), "")), hjust = 0, vjust = -0.5)
    # geom_point(x = as.numeric(current_fulfilled[, , paste("on", scenario, sep = ".")]), y = 0, color = "blue", size = 1.1) +
    # geom_text(aes(label=ifelse((x == as.numeric(current_LUH) & y == 0), as.character("LUH"), "")), hjust = 0, vjust = -0.5) +
    ggtitle(paste0("Marginal return to ", description)) + ylab("Monetary yield gain (USD/ha)") + xlab(unit)

  return(out)
}