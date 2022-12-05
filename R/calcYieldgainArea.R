#' @title       calcYieldgainArea
#' @description reports potentially irrigated area depending on gainthreshold
#'              and land constraint only
#'
#' @param rangeGT       Range of gainthreshold for calculation of potentially
#'                      irrigated areas
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years for which irrigatable area is calculated
#' @param iniyear       Initialization year for cropland area
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param unit          Unit of yield improvement potential used as threshold,
#'                      consisting of unit and price aggregation level separated by ":".
#'                      Unit:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return.
#'                      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                      For case of no land protection select "NA"
#'                      or do not specify second part of the argument
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcYieldgainArea(rangeGT = seq(0, 10000, by = 100), scenario = "ssp2")
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames
#' @importFrom stringr str_split
#'
#' @export

calcYieldgainArea <- function(rangeGT, lpjml, selectyears, iniyear,
                              climatetype, yieldcalib, unit,
                              irrigationsystem, landScen, cropmix, multicropping) {

  x <- vector(mode = "list", length = length(rangeGT))
  i <- 0

  for (gainthreshold in rangeGT) {

    i <- i + 1

    tmp <- calcOutput("IrrigatableAreaUnlimited", gainthreshold = gainthreshold,
                    selectyears = selectyears, iniyear = iniyear,
                    lpjml = lpjml, climatetype = climatetype,
                    cropmix = cropmix, yieldcalib = yieldcalib, irrigationsystem = irrigationsystem,
                    unit = unit, multicropping = multicropping,
                    landScen = landScen, aggregate = FALSE)

    tmp <- add_dimension(tmp, dim = 3.1, add = "GT", nm = as.character(gainthreshold))

    x[[i]] <- tmp
  }

  out <- mbind(x)
  out <- collapseNames(out)

  return(list(x            = out,
              weight       = NULL,
              unit         = "Mha",
              description  = "Potentially irrigated area only considering land constraint",
              isocountries = FALSE))
}
