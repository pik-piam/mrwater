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
#'                      USD_ha (USD per hectare) for area return, or
#'                      USD_m3 (USD per cubic meter) for volumetric return;
#'                      and boolean indicating fullpotential (TRUE) or reduced potential (FALSE)
#' @param cropmix       Selected cropmix (options:
#'                      "hist_irrig" for historical cropmix on currently irrigated area,
#'                      "hist_total" for historical cropmix on total cropland,
#'                      or selection of proxycrops)
#' @param iniyear       Initialization year for price
#' @param yieldcalib    If TRUE: LPJmL yields calibrated to FAO country yield in iniyear
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
calcIrrigCellranking <- function(lpjml, climatetype, cellrankyear,
                                 method, cropmix, iniyear, yieldcalib, multicropping) {

  fullpotential <- as.logical(strsplit(method, ":")[[1]][2])
  unit          <- strsplit(method, ":")[[1]][1]

  # Read in average potential yield gain per cell (USD05 per ha)
  yieldGain <- calcOutput("IrrigYieldImprovementPotential", unit = unit,
                           selectyears = cellrankyear, iniyear = iniyear,
                          lpjml = lpjml, climatetype = climatetype,
                           cropmix = cropmix, yieldcalib = yieldcalib,
                          multicropping = multicropping, aggregate = FALSE)

  if (!fullpotential) {

    yieldGainReduced           <- 0.75 * yieldGain
    getCells(yieldGain)        <- paste0("A_", getCells(yieldGain))
    getCells(yieldGainReduced) <- paste0("B_", getCells(yieldGainReduced))

    yieldGain <- mbind(yieldGain, yieldGainReduced)

  }

  if (multicropping) {

    single           <- collapseNames(yieldGain[, , "single"])
    getCells(single) <- paste0("S_", getCells(single))
    double           <- collapseNames(yieldGain[, , "double"])
    getCells(double) <- paste0("D_", getCells(double))
    triple           <- collapseNames(yieldGain[, , "triple"])
    getCells(triple) <- paste0("T_", getCells(triple))

    yieldGain <- mbind(single, double, triple)

  } else {

    yieldGain <- collapseNames(yieldGain[, , "single"])

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
              description  = "Rank of cell according to yield gain potential by irrigation",
              isocountries = FALSE))
}
