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
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
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

  if (!is.logical(multicropping)) {
    stop("calcIrrigCropYieldGain requires logical
         in multicropping argument.")
  }

  if (multicropping) {
    # yield gain should only be calculated for "potential"
    multicropping <- "TRUE:potential:endogenous"
  }

  # read in cellular lpjml yields
  yields   <- calcOutput("YieldsValued",
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          priceAgg = priceAgg,
                          multicropping = multicropping,
                          aggregate = FALSE)

  # calculate yield gain per crop
  yieldGain <- (collapseNames(yields[, , "irrigated"]) -
                  collapseNames(yields[, , "rainfed"]))

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
