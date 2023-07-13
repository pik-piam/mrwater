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
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from readLanduseToolbox
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUseCommittedAg", aggregate = FALSE)
#' }
#'
calcWaterUseCommittedAg <- function(lpjml, climatetype,
                                    selectyears, iniyear,
                                    multicropping) {

  # Irrigation water requirements per cell per crop given irrigation
  # system initialization (in m^3 per hectare per year)
  irrigReq <- calcOutput("ActualIrrigWatRequirements",
                         irrigationsystem = "initialization",
                         selectyears = selectyears, iniyear = iniyear,
                         lpjml = lpjml, climatetype = climatetype,
                         multicropping = multicropping, aggregate = FALSE)

  # Read in cropland area (by crop) from crop area initialization (in mio. ha)
  grownCrops <- calcOutput("IrrigAreaCommitted",
                           selectyears = selectyears, iniyear = iniyear,
                           aggregate = FALSE)

  # Committed agricultural uses (in mio. m^3 per year)
  comAg <- irrigReq * grownCrops

  return(list(x            = comAg,
              weight       = NULL,
              unit         = "mio. m^3 per year",
              description  = "agricultural water demands per crop given selected cropmix",
              isocountries = FALSE))
}
