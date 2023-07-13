#' @title       calcActualIrrigWatRequirements
#' @description This function calculates actual irrigation water requirements
#'              per cell given the chosen irrigation system
#'
#' @param lpjml             LPJmL version required for respective inputs: natveg or crop
#' @param selectyears       Years to be returned
#' @param climatetype       Switch between different climate scenarios or historical baseline "GSWP3-W5E5:historical"
#' @param iniyear           Initialization year (for weight by cropland)
#' @param irrigationsystem  Irrigation system used: system share as in initialization year,
#'                          or drip, surface, sprinkler for full irrigation by selected system
#' @param multicropping     Multicropping activated (TRUE) or not (FALSE) and
#'                          Multiple Cropping Suitability mask selected
#'                          (mask can be:
#'                          "none": no mask applied (only for development purposes)
#'                          "actual:total": currently multicropped areas calculated from total harvested areas
#'                                          and total physical areas per cell from readLanduseToolbox
#'                          "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                          "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                          "potential:endogenous": potentially multicropped areas given
#'                                                  temperature and productivity limits
#'                          "potential:exogenous": potentially multicropped areas given
#'                                                 GAEZ suitability classification)
#'                          (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @seealso
#' \code{\link{calcIrrigationSystem}}, \code{\link{calcIrrigWatRequirements}}
#'
#' @examples
#' \dontrun{
#' calcOutput("ActualIrrigWatRequirements", aggregate = FALSE)
#' }
#'
#' @importFrom magclass dimSums collapseNames getNames getCells
#' @importFrom madrat calcOutput

calcActualIrrigWatRequirements <- function(selectyears, iniyear,
                                           lpjml, climatetype,
                                           irrigationsystem, multicropping) {

  # irrigation water requirement per crop per system (in m^3 per ha per yr)
  irrigReq   <- calcOutput("IrrigWatRequirements", selectyears = selectyears,
                           lpjml = lpjml,  climatetype = climatetype,
                           multicropping = multicropping,
                           aggregate = FALSE)
  cellorder  <- getCells(irrigReq)

  # calculate irrigation water requirements per crop [in mio. m^3 per year] given irrigation system share in use
  if (irrigationsystem == "initialization") {

    # Irrigation system area share per crop
    irrigSystemShr <- setYears(calcOutput("IrrigSystemShr", iniyear = iniyear,
                                          aggregate = FALSE),
                               NULL)

    # total irrigation water requirements per crop given
    # irrigation system share (in m^3 per ha per yr)
    irrigReq       <- dimSums(irrigReq[, , getNames(irrigSystemShr)] * irrigSystemShr,
                              dim = "system")

  } else {

    # whole area irrigated by one system as selected in argument "irrigationsystem"
    irrigReq <- collapseNames(irrigReq[, , irrigationsystem])

  }

  # Cell ordering
  irrigReq <- irrigReq[cellorder, , ]

  # Check for NAs and negative values
  if (any(is.na(irrigReq))) {
    stop("Problem in calcActualIrrigWatRequirements:
         produced NA irrigation water requirements")
  }
  if (any(irrigReq < 0)) {
    stop("Problem in calcActualIrrigWatRequirements:
         produced negative irrigation water requirements")
  }

  # Weight: irrigated area (only used for aggregation)
  irrigArea <- setYears(calcOutput("CropareaAdjusted", iniyear = iniyear,
                                   aggregate = FALSE),
                        NULL)

  # Small additive term to account for places with 0 irrigated area
  irrigArea <- collapseNames(irrigArea[, , "irrigated"]) + 1e-9

  return(list(x            = irrigReq,
              weight       = irrigArea,
              unit         = "m^3 per ha per yr",
              description  = "Irrigation water requirements
                             for different crop types
                             under selected irrigation system share
                             per cell and crop",
              isocountries = FALSE))
}
