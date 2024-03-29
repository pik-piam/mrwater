#' @title       calcYieldImprovementPotential
#' @description This function calculates the yield improvement potential
#'              through irrigation or multiple cropping per grid cell
#'              for a given cropmix
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
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
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
#'                                      and total physical areas per cell from LandInG
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
#' calcOutput("YieldImprovementPotential", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells dimSums

calcYieldImprovementPotential <- function(lpjml, climatetype, yieldgaintype, priceAgg,
                                          iniyear, selectyears, cropmix,
                                          yieldcalib, multicropping) {

  # read in yield gain
  yieldGain <- calcOutput("CropYieldGain", priceAgg = priceAgg, yieldgaintype = yieldgaintype,
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)

  if (any(yieldGain < 0)) {
    warning("There are negative yield gains for certain crops. These are set to 0.
            Under single cropping, this is possible when the growing period is shifted
            together with irrigation.")

    # set negative yield gains to 0
    yieldGain[yieldGain < 0] <- 0
    # (Note: irrigation may lead to shift in growing period -> can have negative values
    # (in cropping calendar version);
    # also: under N-stress, irrigation may lead to lower yields,
    # (the latter is only relevant for limited-N-LPJmL version, default: unlimited N))
  }

  # Selected crops
  if (!is.null(cropmix)) {

    # share of crop area by crop type
    cropareaShr <- calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
                              aggregate = FALSE)

    # average (rf/irr) yields over crops weighted with their croparea share
    yieldGain <- dimSums(yieldGain * cropareaShr, dim = "crop")

    # description of output
    description <- "Yield improvement potential through irrigation
                    given cropmix croparea share"

  } else {

    description <- "Yield improvement potential through irrigation
                    for all different crop types"

  }

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  # Check for negatives
  if (any(round(yieldGain, digits = 3) < 0)) {
    stop("Function YieldImprovementPotential produced negative values")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = "USD per hectare",
              description  = description,
              isocountries = FALSE))
}
