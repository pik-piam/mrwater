#' @title       calcYieldgainWatUse
#' @description This function calculates water use on selected land areas
#'              without considering water constraints
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years to be returned
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Year of initialization for cropland area
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param unit          Unit of yield improvement potential to be returned and
#'                      level of price aggregation used, separated by ":".
#'                      Unit:
#'                      USD_ha (USD per hectare) for relative area return, or
#'                      USD_m3 (USD per cubic meter) for relative volumetric return;
#'                      USD for absolute return (total profit);
#'                      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                      For case of no land protection select "NA"
#'                      or do not specify second part of the argument
#' @param rangeGT       Range of gainthreshold for calculation of potentially
#'                      irrigated areas (in USD per hectare)
#'
#' @importFrom magclass collapseNames dimSums getNames mbind
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("YieldgainWatUse", aggregate = FALSE)
#' }
#'
calcYieldgainWatUse <- function(lpjml, climatetype, selectyears, iniyear, landScen,
                                cropmix, yieldcalib, multicropping, unit, irrigationsystem,
                                rangeGT) {

  # Irrigation system area share per crop
  irrigSystemShr <- calcOutput("IrrigSystemShr", iniyear = iniyear, aggregate = FALSE)

  # Irrigation water requirements per crop (in m^3 per hectare per year) [smoothed and harmonized]
  irrigWatRequ   <- calcOutput("IrrigWatRequirements", selectyears = selectyears,
                               lpjml = lpjml, climatetype = climatetype,
                               multicropping = multicropping,
                               aggregate = FALSE)

  ### Area to be irrigated ###
  # Area that can potentially be irrigated according to chosen scenario
  potArea    <- calcOutput("YieldgainArea", rangeGT = rangeGT,
                           lpjml = lpjml, climatetype = climatetype,
                           selectyears = selectyears, iniyear = iniyear,
                           irrigationsystem = irrigationsystem,
                           cropmix = cropmix,
                           multicropping = as.logical(stringr::str_split(multicropping, ":")[[1]][1]),
                           yieldcalib = yieldcalib, unit = unit,
                           landScen = landScen, aggregate = FALSE)

  # Crop share per cell according to chosen cropmix
  cropShr    <- calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
                           aggregate = FALSE)

  # Loop over several gain thresholds
  x <- vector(mode = "list", length = length(rangeGT))
  i <- 0

  for (gainthreshold in rangeGT) {

    i <- i + 1

    tmp <- potArea[, , as.character(gainthreshold)]

    # New cropped area by crop (in mio. ha)
    grownCrops <- cropShr * tmp

    # Agricultural water use (withdrawals and consumption)
    watAgUse <- dimSums(grownCrops * irrigSystemShr * irrigWatRequ,
                        dim = c("crop", "system"))

    x[[i]]   <- watAgUse
  }

  out <- mbind(x)

  # transform unit from mio. m^3 to km^3:
  # (1 km^3 = 1e+09 m^3)
  # (1 mio. = 1e+06)
  out <- out / 1000

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3 per year",
              description  = "total agricultural water use
                              on yield gain areas
                              for chosen cropmix",
              isocountries = FALSE))
}
