#' @title       calcWaterUseCommittedAg
#' @description This function calculates committed agricultural water uses that
#'              are used in the river routing algorithm for distributing
#'              available water across the basin
#'
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param selectyears   Years to be returned
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param iniyear       Year of initialization for cropland area
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
#'
#' @importFrom magclass collapseNames dimSums getNames mbind getRegions
#' @importFrom madrat calcOutput toolAggregate
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUseCommittedAg", aggregate = FALSE)
#' }
#'
calcWaterUseCommittedAg <- function(lpjml, climatetype, selectyears, iniyear,
                                    multicropping) {

  # Irrigation water requirements per cell per crop given irrigation
  # system initialization (in m^3 per hectare per year)
  irrigReq <- calcOutput("ActualIrrigWatRequirements",
                         irrigationsystem = "initialization",
                         selectyears = selectyears, iniyear = iniyear,
                         lpjml = lpjml, climatetype = climatetype,
                         multicropping = multicropping, aggregate = FALSE)

  # Read in cropland area (by crop) from crop area initialization (in mio. ha)
  grownCrops <- calcOutput("IrrigAreaCommitted", selectyears = selectyears,
                           iniyear = iniyear, aggregate = FALSE)

  # Committed agricultural uses (in mio. m^3 per year)
  comAg <- irrigReq * grownCrops

  return(list(x            = comAg,
              weight       = NULL,
              unit         = "mio. m^3 per year",
              description  = "agricultural water demands per crop given current cropmix",
              isocountries = FALSE))
}
