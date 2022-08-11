#' @title       calcCropYieldGain
#' @description This function calculates the yield gains per crop through
#'              irrigation or multiple cropping relative to rainfed-single system
#'              production (including negatives)
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param yieldgaintype Crop yield gain through "multicropping", "irrigation" or "both"
#' @param selectyears   Years to be returned by the function
#' @param unit          Unit of yield improvement potential to be returned and
#'                      level of price aggregation used, separated by ":".
#'                      Unit:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return.
#'                      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices
#' @param iniyear       initialization year for food price and cropmix area
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:cropIrrig" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigCropYieldGain", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcCropYieldGain <- function(lpjml, climatetype, yieldgaintype, unit,
                              iniyear, selectyears,
                              yieldcalib, cropmix, multicropping) {

  # reference yield (rainfed-single cropping)
  ref <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, unit = unit,
                          multicropping = FALSE, cropmix = cropmix,
                          aggregate = FALSE)[, , "rainfed"])

  if (yieldgaintype == "irrigation") {

    # irrigated single-cropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = unit,
                                       multicropping = FALSE, cropmix = cropmix,
                                       aggregate = FALSE)[, , "irrigated"])

  } else if (yieldgaintype == "multicropping") {

    # rainfed multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = unit,
                                       multicropping = multicropping, cropmix = cropmix,
                                       aggregate = FALSE)[, , "rainfed"])

  } else if (yieldgaintype == "irrigation_multicropping") {

    # irrigated multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                            iniyear = iniyear, selectyears = selectyears,
                            yieldcalib = yieldcalib, unit = unit,
                            multicropping = multicropping, cropmix = cropmix,
                            aggregate = FALSE)[, , "irrigated"])

  }

  # calculate yield gain per crop
  yieldGain <- yields - ref

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function IrrigCropYieldGain produced NAs")
  }

  if (any(yieldGain < 0)) {
    warning("There are negative yield gains for certain crops. These are set to 0.
            Under single cropping, this is possible when the growing period is shifted
            together with irrigation.")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = unit,
              description  = paste0("Yield gain potential through",
                                    yieldgaintype,
                                    " for all different crop types"),
              isocountries = FALSE))
}
