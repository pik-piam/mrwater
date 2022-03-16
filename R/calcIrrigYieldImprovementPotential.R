#' @title       calcIrrigYieldImprovementPotential
#' @description This function calculates the yield improvement potential
#'              through irrigation for different crops
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned by the function
#' @param unit          Unit of yield improvement potential to be returned:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return
#' @param iniyear       initialization year for food price and cropmix area
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
#'                      If FALSE: uncalibrated LPJmL yields are used
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigYieldImprovementPotential", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames getCells dimSums

calcIrrigYieldImprovementPotential <- function(lpjml, climatetype, unit,
                                               iniyear, selectyears, cropmix,
                                               yieldcalib, multicropping) {

  # read in yield gain
  yieldGain <- calcOutput("IrrigCropYieldGain", unit = unit,
                          lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, cropmix = cropmix,
                          multicropping = multicropping, aggregate = FALSE)

  # set negative yield gains to 0
  yieldGain[yieldGain < 0] <- 0
  # (Note: irrigation may lead to shift in growing period -> can have negative values
  # (in cropping calendar version);
  # also: under N-stress, irrigation may lead to lower yields,
  # (the latter is only relevant for limited-N-LPJmL version, default: unlimited N))

  # Selected crops
  if (!is.null(cropmix)) {

    # share of crop area by crop type
    cropareaShr <- setYears(calcOutput("CropAreaShare", iniyear = iniyear, cropmix = cropmix,
                              aggregate = FALSE),
                            iniyear)

    # average (rf/irr) yields over crops weighted with their croparea share
    yieldGain <- dimSums(yieldGain * cropareaShr, dim = "crop")

    # description of output
    description <- "Yield improvement potential through irrigation given cropmix croparea share"

  } else {

    description <- "Yield improvement potential through irrigation for all different crop types"

  }

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function YieldImprovementPotential produced NAs")
  }

  # Check for negatives
  if (any(round(yieldGain) < 0)) {
    stop("Function YieldImprovementPotential produced negative values")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = unit,
              description  = description,
              isocountries = FALSE))
}
