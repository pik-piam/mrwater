#' @title       calcPotentialIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements
#'              per cell and season given a certain irrigation system
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years to be returned
#' @param climatetype   Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Initialization year (for weight by cropland)
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
                                              lpjml, climatetype) {

  # Standard settings
  irrigwattype      <- "withdrawal"
  potential_wat     <- TRUE
  com_ag            <- TRUE
  multicropping     <- FALSE
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

  # irrigation water requirement per crop per system per season (in m^3 per ha per yr)
  irrigReq   <- collapseNames(calcOutput("IrrigWatRequirements", selectyears = selectyears,
                           lpjml = lpjml,  climatetype = climatetype,
                           multicropping = multicropping, aggregate = FALSE)[, , irrigwattype])
  irrigReq   <- irrigReq[, , "pasture", invert = TRUE]

  #### HERE: irrigation system assumption for different irrigation efficiencies in MAgPIE ####
  # irrigation system share (share of irrigated area) [PLACEHOLDER: initialization]
  irrigSystemShr <- calcOutput("IrrigationSystem", datasource = "Jaegermeyr",
                                aggregate = FALSE)
  #### HERE: irrigation system assumption for different irrigation efficiencies in MAgPIE ####

  # total irrigation water requirements per crop given irrigation system share (in m^3 per ha per yr)
  irrigReq       <- dimSums(irrigSystemShr * irrigReq,
                            dim = "system")

  # sum over seasons
  if (multicropping) {
    ### HERE: include multiple cropping mask (?????) ###
    irrigReq <- dimSums(irrigReq, dim = "season")
  } else {

    # irrigation water requirements in main season
    irrigReq <- dimSums(irrigReq[, , "first"], dim = "season")

  }

  # Check for NAs and negative values
  if (any(is.na(irrigReq))) {
    stop("produced NA irrigation water requirements")
  }
  if (any(irrigReq < 0)) {
    stop("produced negative irrigation water requirements")
  }

  # Weight: potentially irrigated area (only used for aggregation)
  irrigArea <- collapseNames(calcOutput("IrrigAreaPotential", gainthreshold = gainthreshold,
                                  selectyears = selectyears, iniyear = iniyear,
                                  climatetype = climatetype, lpjml = lpjml,
                                  accessibilityrule = accessibilityrule, efrMethod = efrMethod,
                                  rankmethod = rankmethod, thresholdtype = thresholdtype,
                                  yieldcalib = yieldcalib, allocationrule = allocationrule,
                                  irrigationsystem = irrigationsystem,
                                  landScen = landScen, cropmix = cropmix, potential_wat = potential_wat,
                                  com_ag = com_ag, multicropping = multicropping,
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
