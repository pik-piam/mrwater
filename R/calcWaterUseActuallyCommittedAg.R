#' @title       calcWaterUseActuallyCommittedAg
#' @description This function calculates committed agricultural water uses that
#'              are used in the river routing algorithm for distributing
#'              available water across the basin
#'
#' @param selectyears   Years to be returned
#' @param iniyear       Year of initialization for cropland area
#' @param lpjml         LPJmL version required for respective inputs: natveg or crop
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param efrMethod     EFR method used including selected strictness of EFRs (e.g. Smakhtin:good, VMF:fair)
#' @param fossilGW      If TRUE: non-renewable groundwater can be used.
#'                      If FALSE: non-renewable groundwater cannot be used.
#' @param multicropping Multicropping activated (TRUE) or not (FALSE) and
#'                      Multiple Cropping Suitability mask selected
#'                      (mask can be:
#'                      "none": no mask applied (only for development purposes)
#'                      "actual:total": currently multicropped areas calculated from total harvested areas
#'                                      and total physical areas per cell from LandInG
#'                      "actual:crop" (crop-specific), "actual:irrigation" (irrigation-specific),
#'                      "actual:irrig_crop" (crop- and irrigation-specific) "total"
#'                      "potential:endogenous": potentially multicropped areas given
#'                                              temperature and productivity limits
#'                      "potential:exogenous": potentially multicropped areas given
#'                                             GAEZ suitability classification)
#'                      (e.g. TRUE:actual:total; TRUE:none; FALSE)
#' @param transDist     Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#' @param iteration     Default: "committed_agriculture",
#'                      Special case: "committed_agriculture_fullPotential".
#'                      Special case should only be used for calculation of
#'                      full multicropping potential committed agricultural area
#'                      for case of Current Irrigation.
#'
#' @importFrom stringr str_split
#' @importFrom madrat calcOutput
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("WaterUseActuallyCommittedAg", aggregate = FALSE)
#' }

calcWaterUseActuallyCommittedAg <- function(iteration = "committed_agriculture",
                                            lpjml, climatetype,
                                            selectyears, iniyear,
                                            multicropping, efrMethod,
                                            fossilGW, transDist) {

  # For currently committed agricultural uses, water requirements and areas
  # should be based on current multiple cropping
  if (as.logical(stringr::str_split(multicropping, ":")[[1]][1])) {
    if (grepl(pattern = "fullPotential", x = iteration)) {
      m <- multicropping
    } else {
      m <- "TRUE:actual:irrig_crop"
    }
  } else {
    m <- FALSE
  }

  # Irrigation water requirements per cell per crop given irrigation
  # system initialization (in m^3 per hectare per year)
  irrigReq <- calcOutput("ActualIrrigWatRequirements",
                         irrigationsystem = "initialization",
                         selectyears = selectyears, iniyear = iniyear,
                         lpjml = lpjml, climatetype = climatetype,
                         multicropping = m, aggregate = FALSE)

  # Read in cropland area (by crop) from crop area initialization (in mio. ha)
  grownCrops <- calcOutput("IrrigAreaActuallyCommitted",
                           selectyears = selectyears, iniyear = iniyear,
                           lpjml = lpjml, climatetype = climatetype,
                           efrMethod = efrMethod, multicropping = m,
                           transDist = transDist, fossilGW = fossilGW,
                           aggregate = FALSE)

  # Committed agricultural uses (in mio. m^3 per year)
  comAg <- irrigReq * grownCrops

  return(list(x            = comAg,
              weight       = NULL,
              unit         = "mio. m^3 per year",
              description  = paste0("water use actually committed to agriculture ",
                                    "per crop given currently irrigated area and ",
                                    "(renewable and non-renewable) water availability"),
              isocountries = FALSE))
}
