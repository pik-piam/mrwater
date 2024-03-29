#' @title       calcIrrigatableAreaUnlimited
#' @description calculates area that can potentially be irrigated given chosen
#'              land scenario and gainthreshold
#'
#' @param selectyears   years for which irrigatable area is calculated
#' @param iniyear       initialization year
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, or one of the scenarios available in calcConservationPriorities,
#'                         e.g., 30by20, BH, BH_IFL, PBL_HalfEarth,
#'                         or NA for no protection).
#'                      For case of no land protection select "NA" in second part of argument
#'                      or do not specify second part of the argument
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
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
#' @param gainthreshold     Threshold of yield improvement potential
#'                          (in USD per hectare)
#' @param irrigationsystem  Irrigation system used: system share as in initialization year,
#'                          or drip, surface, sprinkler for full irrigation by selected system
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("IrrigatableAreaUnlimited", aggregate = FALSE)
#' }
#'
#' @importFrom madrat calcOutput

calcIrrigatableAreaUnlimited <- function(selectyears, iniyear, landScen, lpjml,
                                         climatetype, cropmix, yieldcalib, irrigationsystem,
                                         unit, gainthreshold, multicropping) {

  if (!is.logical(multicropping)) {
    stop("calcIrrigatableAreaUnlimited requires logical
         in multicropping argument.")
  }

  # Area that can potentially be irrigated (including total potentially
  # irrigatable area; defined by comagyear=NULL)
  potArea <- calcOutput("AreaPotIrrig", selectyears = selectyears, iniyear = iniyear,
                        landScen = landScen, comagyear = NULL,
                        aggregate = FALSE)

  # Yield gain potential through irrigation of proxy crops (in USD per ha)
  potGain <- calcOutput("IrrigYieldImprovementPotential",
                        unit = paste("USD_ha", unlist(strsplit(unit, split = ":"))[2], sep = ":"),
                        selectyears = selectyears, iniyear = iniyear,
                        lpjml = lpjml, climatetype = climatetype,
                        comagyear = NULL,
                        irrigationsystem = irrigationsystem,
                        landScen = landScen, cropmix = cropmix,
                        yieldcalib = yieldcalib,
                        multicropping = multicropping,
                        aggregate = FALSE)

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
              unit         = "Mha",
              description  = paste0("Area that could be irrigated when water would not be ",
                             "a limiting factor given chosen gainthreshold and land constraint"),
              isocountries = FALSE))
}
