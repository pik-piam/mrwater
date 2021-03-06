#' @title       calcIrrigatableAreaUnlimited
#' @description calculates area that can potentially be irrigated given chosen
#'              land scenario and gainthreshold
#'
#' @param selectyears   years for which irrigatable area is calculated
#' @param avlland_scen  Land availability scenario: current or potential;
#'                      optional additionally: protection scenario in case of potential
#'                      and initialization year of cropland area
#'                      combination of land availability scenario and initialization year separated by ":"
#'                      land availability scenario: currIrrig, currCropland, potIrrig
#'                      protection scenario separated by "_" (only relevant when potIrrig selected):
#'                      WDPA, BH, FF, CPD, LW, HalfEarth
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param cropmix       cropmix for which irrigation yield improvement is calculated
#'                      can be selection of proxycrop(s) for calculation of average yield gain
#'                      or hist_irrig or hist_total for historical cropmix
#' @param yieldcalib    FAO (LPJmL yields calibrated with current FAO yield) or
#'                      calibrated (LPJmL yield potentials harmonized to baseline and calibrated for proxycrops) or
#'                      smoothed (smoothed LPJmL yield potentials, not harmonized, not calibrated) or
#'                      smoothed_calibrated (smoothed LPJmL yield potentials, not harmonized, calibrated for proxycrops)
#' @param thresholdtype Unit of yield improvement potential used as threshold:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return
#' @param gainthreshold Threshold of yield improvement potential
#'                      (same unit as thresholdtype)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigatableArea", aggregate = FALSE)
#' }
#'
#' @import magclass
#' @import magpiesets

calcIrrigatableAreaUnlimited <- function(selectyears, avlland_scen, lpjml,
                                         climatetype, cropmix, yieldcalib,
                                         thresholdtype, gainthreshold, multicropping) {

  # Retrieve function arguments
  iniyear       <- as.numeric(as.list(strsplit(avlland_scen, split = ":"))[[1]][2])

  ## Area that can potentially be irrigated (including total potentially irrigatable area; defined by comagyear=NULL)
  potArea <- calcOutput("AreaPotIrrig", selectyears = selectyears,
                         avlland_scen = avlland_scen, comagyear = NULL, aggregate = FALSE)

  # Yield gain potential through irrigation of proxy crops
  potGain <- calcOutput("IrrigYieldImprovementPotential", unit = thresholdtype,
                         lpjml = lpjml, climatetype = climatetype, cropmix = cropmix,
                         selectyears = selectyears, iniyear = iniyear,
                         yieldcalib = yieldcalib, multicropping = multicropping, aggregate = FALSE)

  # sum over all seasons
  potGain <- dimSums(potGain, dim = 3)

  # remove areas below chosen gainthreshold
  potArea[potGain < gainthreshold] <- 0

  # check for NAs and negative values
  if (any(is.na(potArea))) {
    stop("produced NA irrigatable area")
  }

  if (any(potArea < 0)) {
    stop("produced negative irrigatable area")
  }

  return(list(x            = potArea,
              weight       = NULL,
              unit         = "mio. ha",
              description  = "Area that would be irrigated given chosen gainthreshold and land constraint",
              isocountries = FALSE))
}
