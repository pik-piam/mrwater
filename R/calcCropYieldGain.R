#' @title       calcCropYieldGain
#' @description This function calculates the yield gains per crop through
#'              irrigation or multiple cropping relative to rainfed-single system
#'              production (including negatives)
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param yieldgaintype Crop yield gain through
#'                      multiple cropping under rainfed conditions "multicropping_rf",
#'                      multiple cropping under irrigated conditions "multicropping_ir",
#'                      irrigation under single cropping conditions "irrigation_singlecropping"
#'                      irrigation and multiple cropping "irrigation_multicropping"
#' @param selectyears   Years to be returned by the function
#' @param priceAgg      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices, or
#'                      "CONST" for same price for all crops
#' @param iniyear       initialization year for food price and cropmix area
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                               Also needs specification of refYields, separated by ":".
#'                               Options: FALSE (for single cropping analyses) or
#'                                        "TRUE:actual:irrig_crop" (for multiple cropping analyses)
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
#' calcOutput("CropYieldGain", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcCropYieldGain <- function(lpjml, climatetype, yieldgaintype, priceAgg,
                              iniyear, selectyears,
                              yieldcalib,
                              cropmix, multicropping) {

  # reference yield (rainfed-single cropping)
  ref <- calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, unit = priceAgg,
                          multicropping = FALSE, cropmix = cropmix,
                          aggregate = FALSE)

  if (yieldgaintype == "irrigation_singlecropping") {

    # irrigated yield (single cropping)
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = priceAgg,
                                       multicropping = FALSE, cropmix = cropmix,
                                       aggregate = FALSE)[, , "irrigated"])

    # yield gain through irrigation under single cropping conditions
    yieldGain <- yields - collapseNames(ref[, , "rainfed"])

  } else if (yieldgaintype == "irrigation_multicropping") {

    # irrigated multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = priceAgg,
                                       multicropping = multicropping, cropmix = cropmix,
                                       aggregate = FALSE)[, , "irrigated"])
    # yield gain through multiple cropping under irrigated conditions
    yieldGain <- yields - collapseNames(ref[, , "irrigated"])

  } else if (yieldgaintype == "multicropping_rf") {

    # rainfed multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = priceAgg,
                                       multicropping = multicropping, cropmix = cropmix,
                                       aggregate = FALSE)[, , "rainfed"])
    # yield gain through multiple cropping under rainfed conditions
    yieldGain <- yields - collapseNames(ref[, , "rainfed"])

  } else if (yieldgaintype == "multicropping_ir") {

    # irrigated multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                                       iniyear = iniyear, selectyears = selectyears,
                                       yieldcalib = yieldcalib, unit = priceAgg,
                                       multicropping = multicropping, cropmix = cropmix,
                                       aggregate = FALSE)[, , "irrigated"])

    # yield gain through multiple cropping under irrigated conditions
    yieldGain <- yields - collapseNames(ref[, , "irrigated"])

  } else if (yieldgaintype == "irrigation_and_multicropping") {

    # irrigated multicropped yields
    yields <- collapseNames(calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                            iniyear = iniyear, selectyears = selectyears,
                            yieldcalib = yieldcalib, unit = priceAgg,
                            multicropping = multicropping, cropmix = cropmix,
                            aggregate = FALSE)[, , "irrigated"])

    # yield gain through irrigation & multiple cropping
    yieldGain <- yields - collapseNames(ref[, , "rainfed"])

  } else {
    stop("Please select yieldgaintype to be returned by calcCropYieldGain.R
         Options are: irrigation_singlecropping, multicropping_rf, multicropping_ir,
         irrigation_multicropping")
  }

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function IrrigCropYieldGain produced NAs")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = "USD per hectare",
              description  = paste0("Yield gain potential through ",
                                    yieldgaintype,
                                    " for all different crop types"),
              isocountries = FALSE))
}
