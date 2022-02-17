#' @title       calcIrrigCropYieldGain
#' @description This function calculates the yield gains per crop and season
#'              (not accounting for multiple cropping zones and including negatives)
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
#' calcOutput("IrrigCropYieldGain", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames

calcIrrigCropYieldGain <- function(lpjml, climatetype, unit,
                                   iniyear, selectyears, cropmix,
                                   yieldcalib, multicropping) {

  # read in cellular lpjml yields for each season
  yields    <- calcOutput("YieldsValued", lpjml = lpjml, climatetype = climatetype,
                          iniyear = iniyear, selectyears = selectyears,
                          yieldcalib = yieldcalib, unit = unit,
                          multicropping = multicropping, aggregate = FALSE)

  # calculate yield gain per crop per season
  yieldGain <- (collapseNames(yields[, , "irrigated"]) -
                  collapseNames(yields[, , "rainfed"]))

  # Check for NAs
  if (any(is.na(yieldGain))) {
    stop("Function IrrigCropYieldGain produced NAs")
  }

  return(list(x            = yieldGain,
              weight       = NULL,
              unit         = unit,
              description  = "Yield gain potential through irrigation for all different crop types",
              isocountries = FALSE))
}
