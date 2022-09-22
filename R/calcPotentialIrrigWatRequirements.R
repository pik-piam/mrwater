#' @title       calcPotentialIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements
#'              per cell given a certain irrigation system
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years to be returned
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year (for weight by cropland)
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; FALSE:NULL)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @seealso
#' \code{\link{calcActualIrrigWatRequirements}}, \code{\link{calcIrrigWatRequirements}}
#'
#' @examples
#' \dontrun{
#' calcOutput("PotentialIrrigWatRequirements", aggregate = FALSE)
#' }
#'
#' @importFrom magclass dimSums collapseNames
#' @importFrom madrat calcOutput

calcPotentialIrrigWatRequirements <- function(selectyears, iniyear,
                                              lpjml, climatetype,
                                              multicropping) {

  # Standard settings
  irrigwattype      <- "withdrawal"
  potentialWat      <- TRUE
  comAg             <- TRUE
  cropmix           <- c("maiz", "rapeseed", "puls_pro")
  landScen          <- "potCropland:HalfEarth"
  irrigationsystem  <- "initialization"

  efrMethod         <- "VMF:fair"
  accessibilityrule <- "CV:2"
  yieldcalib        <- TRUE
  allocationrule    <- "optimization"

  gainthreshold     <- 0
  rankmethod        <- "USD_ha:TRUE"
  thresholdtype     <- "USD_ha"

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrigReq   <- collapseNames(calcOutput("IrrigWatRequirements", selectyears = selectyears,
                           lpjml = lpjml,  climatetype = climatetype,
                           multicropping = multicropping,
                           aggregate = FALSE)[, , irrigwattype])
  cropsystemlist <- getItems(irrigReq, dim = 3)

  # Irrigation system area share per crop
  irrigSystemShr <- calcOutput("IrrigSystemShr", iniyear = iniyear, aggregate = FALSE)

  # total irrigation water requirements per crop given irrigation system share (in m^3 per ha per yr)
  irrigReq       <- dimSums(irrigSystemShr[, , cropsystemlist] * irrigReq,
                            dim = "system")

  # Check for NAs and negative values
  if (any(is.na(irrigReq))) {
    stop("Problem in calcPotentialIrrigWatRequirements:
         produced NA irrigation water requirements")
  }
  if (any(irrigReq < 0)) {
    stop("Problem in calcPotentialIrrigWatRequirements:
         produced negative irrigation water requirements")
  }

  # Weight: potentially irrigated area (only used for aggregation)
  irrigArea <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, thresholdtype = thresholdtype,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potentialWat = potentialWat,
                                  comAg = comAg, multicropping = multicropping,
                                  aggregate = FALSE)[, , "irrigatable"])

  # Small additive term to account for places with 0 potentially irrigated area
  irrigArea <- collapseNames(irrigArea[, , "irrigated"]) + 1e-9

  irrigReq  <- toolLPJcell2MAgPIEcell(irrigReq)
  irrigArea <- toolLPJcell2MAgPIEcell(irrigArea)

  return(list(x            = irrigReq,
              weight       = irrigArea,
              unit         = "m^3 per ha per yr",
              description  = "Irrigation water requirements for irrigation for
                             different crop types under selected irrigation system share per cell",
              isocountries = FALSE))
}
