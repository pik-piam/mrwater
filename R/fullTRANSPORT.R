#' @title fullTRANSPORT
#' @description Function that produces output for analysis of
#'              water transport for provision to cells in surrounding
#'
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                          "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                          separated by ":"
#'                          (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @author Felicitas Beier
#'
#' @importFrom madrat calcOutput
#' @importFrom stringr str_split
#' @export

fullTRANSPORT <- function(multicropping) {

  # Standard settings
  iniyear           <- "y2010"
  selectyears       <- "y2010"
  lpjml             <- c(natveg = "LPJmL4_for_MAgPIE_44ac93de",
                         crop = "ggcmi_phase3_nchecks_9ca735cb")
  climatetype       <- "GSWP3-W5E5:historical"
  cropmix           <- "hist_total"
  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  allocationrule    <- "optimization"
  rankmethod        <- "USD_ha:GLO:TRUE"
  thresholdtype     <- "USD_ha:GLO"
  irrigationsystem  <- "initialization"

  if (as.logical(str_split(multicropping, ":")[[1]][1])) {
    yieldcalib        <- "TRUE:TRUE:actual:irrig_crop"
  } else {
    yieldcalib        <- "TRUE:FALSE"
  }

  ##################################
  ###      Current Croparea      ###
  ##################################
  # Landuse Toolbox Croparea
  # Unit: Mha
  calcOutput("CropareaAdjusted",
             iniyear = iniyear, dataset = "Toolbox",
             aggregate = FALSE,
             file = "cropareaToolbox.mz")

  # Cropmix that was basis for calculations
  calcOutput("CropAreaShare",
             iniyear = iniyear, cropmix = cropmix,
             aggregate = FALSE,
             file = "cropareaShare.mz")

  ##################################
  ###   Yields and Yield Gains   ###
  ##################################
  calcOutput("IrrigCropYieldGain", unit = "USD_ha:GLO",
             lpjml = lpjml, climatetype = climatetype,
             iniyear = iniyear, selectyears = selectyears,
             yieldcalib = yieldcalib, cropmix = cropmix,
             multicropping = multicropping, aggregate = FALSE,
             file = "cropyieldgain.mz")

  ##################################
  ### Potentially Irrigated Area ###
  ##################################
  # Potentially irrigated area for different water provision distances
  # min distance: 0 (no water diversion)
  # max distance: 1000km (comparable to China South-to-North-Water-Diversion-Project 1155km)
  for (transDist in c(0, 100, 200, 300, 500, 1000)) {
    # Current State (Committed Agricultural Area)
    calcOutput("IrrigAreaPotential", gainthreshold = 0,
               landScen = paste0("currIrrig:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               potentialWat = FALSE, comAg = TRUE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0("curpotIrrigArea_", transDist, "km.mz"))

    # Potentially Irrigated Area (with previously committed areas)
    calcOutput("IrrigAreaPotential", gainthreshold = 0,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               potentialWat = FALSE, comAg = TRUE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0("potIrrigArea_", transDist, "km.mz"))

    # Potentially Irrigated Area (without committed areas)
    calcOutput("IrrigAreaPotential", gainthreshold = 0,
               landScen = paste0("currCropland:", "NULL"),
               transDist = transDist,
               selectyears = selectyears, iniyear = iniyear,
               lpjml = lpjml, climatetype = climatetype,
               efrMethod = efrMethod, accessibilityrule = accessibilityrule,
               rankmethod = rankmethod, yieldcalib = yieldcalib,
               allocationrule = allocationrule, thresholdtype = thresholdtype,
               irrigationsystem = irrigationsystem, cropmix = cropmix,
               potentialWat = FALSE, comAg = FALSE,
               multicropping = multicropping, aggregate = FALSE,
               file = paste0("fullpotIrrigArea_", transDist, "km.mz"))
  }
}
