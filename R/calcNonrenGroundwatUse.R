#' @title       calcNonrenGroundwatUse
#' @description Calculates current non-renewable (fossil) groundwater use
#'              as negative difference between
#'              water availability from renewable sources and
#'              human water use (consumption and withdrawal).
#'              The water use in the initialization year is fixed in the future as
#'              non-renewable groundwater.
#'
#' @param output        "total": total groundwater use (non-agricultural and agricultural),
#'                      "nonAg": groundwater use in non-agricultural sector (industry, domestic, electricity),
#'                      "comAg": groundwater use in agricultural sector (currently irrigated area)
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
#'                                      and total physical areas per cell from readLanduseToolbox
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
#' calcOutput("NonrenGroundwatUse", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames dimSums
#' @importFrom magpiesets findset
#'
#' @export

calcNonrenGroundwatUse <- function(output, lpjml, climatetype,
                                   transDistGW = 100, multicropping = "TRUE:actual:irrig_crop",
                                   selectyears, iniyear) {

  # multiple cropping as of current multiple cropping pattern
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

  # Groundwater is calculated based on full water use of the past given all available water (EFP off).
  # In the past, the non-agriculture scenarios are identical. Therefore, ISIMIP is used.
  scenNonAg <- "ISIMIP"
  scenEFP   <- "off"

  # Missing water of past time steps is attributed to non-renewable groundwater.
  # For the future, it is fixed to the missing water of the last time step.
  lastYr <- as.numeric(gsub("y", "", tail(magpiesets::findset("past"), n = 1)))

  if (is.character(selectyears)) {
    selectyears <- as.numeric(gsub("y", "", selectyears))
  }
  if (is.character(iniyear)) {
    iniyear     <- as.numeric(gsub("y", "", iniyear))
  }
  if (iniyear > selectyears[1]) {
    stop(paste0("Please align the initialization year ('iniyear') ",
                "with the selected years ('selectyears'). ",
                "The 'iniyear' has to be the first year of 'selectyears' ",
                "or lie in the past of it."))
  }
  # include iniyear in selectyears
  if (length(intersect(iniyear, selectyears)) < 1) {
    cmbYrs <- c(iniyear, selectyears)
  } else {
    cmbYrs <- selectyears
  }
  # selectyears of the past (including iniyear)
  pstYrs <- cmbYrs[cmbYrs <= lastYr]
  # selectyears of the future
  ftrYrs <- cmbYrs[cmbYrs > lastYr]


  ##############################
  ### Results from Algorithm ###
  ##############################
  # (can be fulfilled with renewable water sources)
  # Water for Non-Agricultural Use after Routing (in mio. m^3)
  fulfilledNAU <- collapseNames(calcOutput("RiverHumanUseAccounting",
                                iteration = "non_agriculture",
                                lpjml = lpjml, climatetype = climatetype,
                                efrMethod = "VMF:fair", transDist = transDist,
                                selectyears = pstYrs, iniyear = iniyear,
                                multicropping = m,
                                accessibilityrule = NULL,
                                rankmethod = NULL, gainthreshold = NULL,
                                cropmix = NULL, yieldcalib = NULL,
                                irrigationsystem = NULL, landScen = NULL,
                                comAg = FALSE,
                                aggregate = FALSE)[, , paste(scenEFP, scenNonAg, sep = ".")])
  fulfilledNAU <- fulfilledNAU[, , c("currHumanWWtotal", "currHumanWCtotal")]
  getItems(fulfilledNAU, dim = 3) <- c("withdrawal", "consumption")

  # Water Committed to Agriculture after Routing (in mio. m^3)
  fulfilledCAU   <- collapseNames(calcOutput("RiverHumanUseAccounting",
                                  iteration = "committed_agriculture",
                                  lpjml = lpjml, climatetype = climatetype,
                                  efrMethod = "VMF:fair", transDist = transDist,
                                  selectyears = pstYrs, iniyear = iniyear,
                                  multicropping = m,
                                  accessibilityrule = NULL,
                                  rankmethod = NULL, gainthreshold = NULL,
                                  cropmix = NULL, yieldcalib = NULL,
                                  irrigationsystem = NULL, landScen = NULL,
                                  comAg = FALSE,
                                  aggregate = FALSE)[, , paste(scenEFP, scenNonAg, sep = ".")])
  fulfilledCAU <- fulfilledCAU[, , c("currHumanWWtotal", "currHumanWCtotal")]
  getItems(fulfilledCAU, dim = 3) <- c("withdrawal", "consumption")

  ##############################
  ### Exogenous Water Demand ###
  ##############################
  # (would be required according to reported current water uses)
  # Committed Agricultural Water (in mio. m^3)
  actCAU <- dimSums(calcOutput("WaterUseCommittedAg",
                                lpjml = lpjml, climatetype = climatetype,
                                selectyears = pstYrs, iniyear = iniyear,
                                multicropping = m, aggregate = FALSE),
                    dim = "crop")

  # Non-Agricultural Water in the past (in mio. m^3 / yr)
  actNAU <- collapseNames(calcOutput("WaterUseNonAg",
                          selectyears = pstYrs, cells = "lpjcell",
                          datasource = "WATERGAP_ISIMIP", usetype = "total",
                          seasonality = "total", harmonType = "average",
                          lpjml = NULL, climatetype = NULL,
                          aggregate = FALSE)[, , scenNonAg])


  ################################
  ### Unfulfilled Water Demand ###
  ################################
  missingNAU <- actNAU - fulfilledNAU
  missingCAU <- actCAU - fulfilledCAU
  missingWat <- missingNAU + missingCAU

  ######################
  ### Prepare output ###
  ######################
  out <- new.magpie(fill = NA,
                    cells_and_regions = getItems(missingCAU, dim = 1),
                    years = selectyears,
                    names = c("withdrawal", "consumption"))
  getSets(out) <- c("x", "y", "iso", "year", "data")

  if (output == "total") {
    out[, pstYrs, ] <- missingWat[, pstYrs, ]
    out[, ftrYrs, ] <- missingWat[, lastYr, ]
  } else if (output == "nonAg") {
    out[, pstYrs, ] <- missingNAU[, pstYrs, ]
    out[, ftrYrs, ] <- missingNAU[, lastYr, ]
  } else if (output == "comAg") {
    out[, pstYrs, ] <- missingCAU[, pstYrs, ]
    out[, ftrYrs, ] <- missingCAU[, lastYr, ]
  } else {
    stop("Please select whether total groundwater use or sector-wise groundwater use
         shall be reported by calcNonrenGroundwaterUse")
  }


  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 4) < 0, na.rm = TRUE)) {
    stop("calcNonrenGroundwatUse produced negative values")
  }
  if (any(is.na(out))) {
    stop("calcNonrenGroundwatUse produced NAs")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "mio. m^3",
              description  = paste0("usable non-renewable groundwater volume ",
                                    "for human consumption and withdrawal"),
              isocountries = FALSE))
}
