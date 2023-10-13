#' @title       calcIrrigCellranking
#' @description This function calculates a cellranking
#'              for the river basin discharge allocation
#'              based on yield improvement potential through irrigation
#'
#' @param lpjml         LPJmL version used for yields
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical" for yields
#' @param cellrankyear  Year(s) for which cell rank is calculated
#' @param method        Rank and optimization method consisting of
#'                      Unit according to which rank is calculated:
#'                      tDM (tons per dry matter),
#'                      USD_ha (USD per hectare) for relative area return, or
#'                      USD_m3 (USD per cubic meter) for relative volumetric return;
#'                      USD for absolute return (total profit);
#'                      Price aggregation:
#'                      "GLO" for global average prices, or
#'                      "ISO" for country-level prices;
#'                      and boolean indicating fullpotential (TRUE) or reduced potential (FALSE)
#' @param efrMethod     if method USD or US_m3: EFR method used to calculate committed
#'                      agricultural use relevant for chosen land scenario (e.g., Smakhtin:good, VMF:fair)
#' @param transDist     if method USD or US_m3: Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      of committed agricultural uses relevant for chosen land scenario
#' @param comagyear     if !NULL: already irrigated area is subtracted;
#'                      if NULL: total potential land area is used;
#'                      year specified here is the year of the initialization
#'                      used for cropland area initialization in calcIrrigatedArea
#' @param fossilGW      In this function, this argument is only relevant when comagyear !NULL
#'                      If TRUE: non-renewable groundwater can be used.
#'                      If FALSE: non-renewable groundwater cannot be used.
#' @param cropmix       Selected cropmix for which yield improvement potential
#'                      is calculated (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#'                      NULL returns all crops individually
#' @param irrigationsystem Irrigation system used: system share as in initialization year,
#'                         or drip, surface, sprinkler for full irrigation by selected system
#' @param landScen      Land availability scenario consisting of two parts separated by ":":
#'                      1. available land scenario (currCropland, currIrrig, potCropland)
#'                      2. protection scenario (WDPA, BH, FF, CPD, LW, HalfEarth, BH_IFL, NA).
#'                      For case of no land protection select "NA"
#'                      or do not specify second part of the argument
#' @param iniyear       Initialization year for price
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
#' calcOutput("IrrigCellranking", aggregate = FALSE)
#' }
#'
calcIrrigCellranking <- function(lpjml, climatetype,
                                 cellrankyear, iniyear,
                                 comagyear, fossilGW,
                                 irrigationsystem, landScen,
                                 method, efrMethod, transDist,
                                 cropmix, yieldcalib,
                                 multicropping) {

  if (!is.logical(multicropping)) {
    stop("calcIrrigCellranking requires logical
         in multicropping argument.")
  }

  fullpotential <- as.logical(strsplit(method, ":")[[1]][3])
  unit          <- paste(strsplit(method, ":")[[1]][1],
                         strsplit(method, ":")[[1]][2],
                         sep = ":")

  # Read in average potential yield gain per cell
  yieldGain <- calcOutput("IrrigYieldImprovementPotential", unit = unit,
                          lpjml = lpjml, climatetype = climatetype,
                          selectyears = cellrankyear, iniyear = iniyear,
                          comagyear = comagyear, fossilGW = fossilGW,
                          efrMethod = efrMethod, transDist = transDist,
                          irrigationsystem = irrigationsystem, landScen = landScen,
                          cropmix = cropmix, yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)

  if (!fullpotential) {

    yieldGainReduced           <- 0.75 * yieldGain
    getCells(yieldGain)        <- paste0("A_", getCells(yieldGain))
    getCells(yieldGainReduced) <- paste0("B_", getCells(yieldGainReduced))

    yieldGain <- mbind(yieldGain, yieldGainReduced)

  }

  # calculate rank (ties are solved by first occurrence)
  glocellrank <- apply(-yieldGain, c(2, 3), rank, ties.method = "first")

  # transform to magpie object
  glocellrank <- as.magpie(glocellrank, spatial = 1)

  # Check for NAs
  if (any(is.na(glocellrank))) {
    stop("Function IrrigCellranking produced NAs")
  }

  return(list(x            = glocellrank,
              weight       = NULL,
              unit         = "1",
              description  = "Rank of cell according to
                              yield gain potential by irrigation",
              isocountries = FALSE))
}
