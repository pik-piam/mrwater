#' @title       calcIrrigCropYieldGain
#' @description This function calculates the yield gains per crop through irrigation
#'              (including negatives)
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
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
#' @importFrom stringr str_split

calcIrrigCropYieldGain <- function(lpjml, climatetype, priceAgg,
                                   iniyear, selectyears,
                                   yieldcalib, multicropping) {

  # read in cellular lpjml yields
  yields   <- calcOutput("YieldsValued",
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          priceAgg = priceAgg,
                          multicropping = multicropping,
                          aggregate = FALSE)

  # Extract multicropping argument
  areaMask      <- paste(str_split(multicropping, ":")[[1]][2],
                         str_split(multicropping, ":")[[1]][3], sep = ":")

  # reference rainfed yield
  if (areaMask == "actual:crop_irrig") {

    # Special case where there might be rainfed potential when there is no irrigated potential
    # Therefore, use potential:endogenous mask for rainfed yields
    refYields   <- calcOutput("YieldsValued",
                           lpjml = lpjml, climatetype = climatetype,
                           iniyear = iniyear, selectyears = selectyears,
                           yieldcalib = yieldcalib,
                           priceAgg = priceAgg,
                           multicropping = "TRUE:potential:endogenous",
                           aggregate = FALSE)
  } else {

    # For other cases, same reference can be used because there is always irrigated potential,
    # when there is rainfed potential
    refYields   <- calcOutput("YieldsValued",
                           lpjml = lpjml, climatetype = climatetype,
                           iniyear = iniyear, selectyears = selectyears,
                           yieldcalib = yieldcalib,
                           priceAgg = priceAgg,
                           multicropping = multicropping,
                           aggregate = FALSE)
  }

  # calculate yield gain per crop
  yieldGain <- (collapseNames(yields[, , "irrigated"]) -
                  collapseNames(refYields[, , "rainfed"]))

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
              unit         = "USD per hectare",
              description  = "Yield gain potential through irrigation
                              per crop type",
              isocountries = FALSE))
}
