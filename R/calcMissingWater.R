#' @title       calcMissingWater
#' @description Calculates difference between requested water
#'              and water that can be fulfilled in algorithm
#'              given available surface water resources
#'              for all scenarios.
#'              This information is used to derive
#'              non-renewable groundwater use.
#'
#' @param output        sector to be reported:
#'                      non-agricultural sector water use ("nonAg"),
#'                      (committed) agricultural water use ("comAg"),
#'                      or both ("total")
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year
#' @param transDistGW   Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
#'                      to determine missing water that is tagged as non-renewable groundwater
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
#'
#' @return cellular magpie object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcOutput("MissingWater", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames dimSums
#' @importFrom magpiesets findset
#' @importFrom stringr str_split
#'
#' @export

calcMissingWater <- function(output, lpjml, climatetype,
                             transDistGW = 100, multicropping = "TRUE:actual:irrig_crop",
                             selectyears, iniyear) {

  # Multiple cropping as of current multiple cropping pattern
  if (as.logical(str_split(multicropping, ":")[[1]][1])) {
    m <- "TRUE:actual:irrig_crop"
  } else {
    m <- as.logical(str_split(multicropping, ":")[[1]][1])
  }

  # Only currently reported water use that cannot be fulfilled
  # even under a chosen transport distance is counted as non-renewable groundwater
  # Note: the transDistGW argument is separate from the transDist argument that can be
  #       used to vary surface water transport distance
  transDist <- transDistGW

  ##############################
  ### Results from Algorithm ###
  ##############################
  # (can be fulfilled with renewable water sources)
  # Water for Non-Agricultural Use after Routing (in mio. m^3)
  fulfilledNAU <- calcOutput("RiverHumanUseAccounting",
                             iteration = "non_agriculture",
                             lpjml = lpjml, climatetype = climatetype,
                             efrMethod = "VMF:fair", transDist = transDist,
                             selectyears = selectyears, iniyear = iniyear,
                             multicropping = m,
                             accessibilityrule = NULL,
                             rankmethod = NULL, gainthreshold = NULL,
                             cropmix = NULL, yieldcalib = NULL,
                             irrigationsystem = NULL, landScen = NULL,
                             comAg = FALSE,
                             aggregate = FALSE)
  fulfilledNAU <- collapseNames(fulfilledNAU[, , c("currHumanWWtotal", "currHumanWCtotal")])
  getItems(fulfilledNAU, dim = 3.3) <- c("withdrawal", "consumption")

  # Water Committed to Agriculture after Routing (in mio. m^3)
  fulfilledCAU   <- calcOutput("RiverHumanUseAccounting",
                               iteration = "committed_agriculture",
                               lpjml = lpjml, climatetype = climatetype,
                               efrMethod = "VMF:fair", transDist = transDist,
                               selectyears = selectyears, iniyear = iniyear,
                               multicropping = m,
                               accessibilityrule = NULL,
                               rankmethod = NULL, gainthreshold = NULL,
                               cropmix = NULL, yieldcalib = NULL,
                               irrigationsystem = NULL, landScen = NULL,
                               comAg = FALSE,
                               aggregate = FALSE)
  fulfilledCAU <- collapseNames(fulfilledCAU[, , c("currHumanWWtotal", "currHumanWCtotal")])
  getItems(fulfilledCAU, dim = 3.3) <- c("withdrawal", "consumption")

  ##############################
  ### Exogenous Water Demand ###
  ##############################
  # (would be required according to reported current water uses)
  # Non-Agricultural Water in the past (in mio. m^3 / yr)
  actNAU <- calcOutput("WaterUseNonAg",
                       selectyears = selectyears, cells = "lpjcell",
                       datasource = "WATERGAP_ISIMIP", usetype = "total",
                       seasonality = "total", harmonType = "average",
                       lpjml = NULL, climatetype = NULL,
                       aggregate = FALSE)
  # Committed Agricultural Water (in mio. m^3)
  # (including depreciation of irrigation equipment)
  actCAU <- dimSums(calcOutput("WaterUseCommittedAg",
                               lpjml = lpjml, climatetype = climatetype,
                               selectyears = selectyears, iniyear = iniyear,
                               multicropping = m, aggregate = FALSE),
                    dim = "crop")

  # Transform object dimensionality
  .transformObject <- function(x, gridcells, years, names) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = gridcells,
                          years = years,
                          names = names,
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen.type"))
    # bring object x to dimension of object0
    out <- object0 + x
    return(out)
  }
  actNAU <- .transformObject(x = actNAU,
                             gridcells = getItems(actNAU, dim = 1),
                             years = selectyears,
                             names = getItems(fulfilledNAU, dim = 3))
  actCAU <- .transformObject(x = actCAU,
                             gridcells = getItems(actCAU, dim = 1),
                             years = selectyears,
                             names = getItems(fulfilledCAU, dim = 3))

  ################################
  ### Unfulfilled Water Demand ###
  ################################
  missingNAU <- actNAU - fulfilledNAU
  missingCAU <- actCAU - fulfilledCAU
  missingWat <- missingNAU + missingCAU

  ##############
  ### Output ###
  ##############
  if (output == "total") {
    out <- missingWat
  } else if (output == "nonAg") {
    out <- missingNAU
  } else if (output == "comAg") {
    out <- missingCAU
  } else {
    stop("Please select whether total groundwater use or sector-wise groundwater use
         shall be reported by mrwater::calcMissingWater")
  }

  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 4) < 0, na.rm = TRUE)) {
    stop("mrwater::calcMissingWater produced negative values")
  }
  if (any(is.na(out))) {
    stop("mrwater::calcMissingWater produced NAs")
  }
  if (any(round(missingCAU, digits = 4) < 0, na.rm = TRUE)) {
    stop("There are negatives in mrwater::calcMissingWater for committed agricultural uses")
  }
  if (any(round(missingNAU, digits = 4) < 0, na.rm = TRUE)) {
    stop("There are negatives in mrwater::calcMissingWater for non-agricultural uses")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("missing water to fulfilled currently observed ",
                                    "(or committed) agricultural or non-agricultural ",
                                    "water consumption and withdrawal"),
              isocountries = FALSE))
}
