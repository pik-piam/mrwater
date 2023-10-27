#' @title       calcShrHumanUsesFulfilled
#' @description calculates of share of current non-agricultural and irrigation
#'              that can be fulfilled
#'              given renewable water availability of the algorithm
#'
#' @param lpjml         LPJmL version used
#' @param climatetype   Switch between different climate scenarios or
#'                      historical baseline "GSWP3-W5E5:historical"
#' @param selectyears   Years to be returned
#' @param iniyear       Initialization year
#' @param efrMethod     EFR method used including selected strictness of EFRs
#'                      (e.g. Smakhtin:good, VMF:fair)
#' @param transDist     Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand by surrounding cell water availability
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
#' calcOutput("ShrHumanUsesFulfilled", aggregate = FALSE)
#' }
#'
#' @importFrom magclass collapseNames dimSums
#'
#' @export

calcShrHumanUsesFulfilled <- function(lpjml, climatetype,
                                      transDist, multicropping,
                                      selectyears, iniyear, efrMethod) {

  ### Reasons for not-fulfilled actually observed irrigation:
  # - fossil groundwater is used for irrigation (e.g. Northern India), but not accounted for in the river routing
  # - deficit irrigation is in place (e.g. Southern Spain), but not accounted for in the river routing

  ##############################
  ### Results from Algorithm ###
  ##############################
  # (can be fulfilled)
  # Water for Non-Agricultural Use after Routing (in mio. m^3)
  fulfilledNAU <- calcOutput("RiverHumanUseAccounting",
                             iteration = "non_agriculture",
                             lpjml = lpjml, climatetype = climatetype,
                             efrMethod = efrMethod, transDist = transDist,
                             selectyears = selectyears, iniyear = iniyear,
                             multicropping = multicropping,
                             accessibilityrule = NULL,
                             rankmethod = NULL, gainthreshold = NULL,
                             cropmix = NULL, yieldcalib = NULL,
                             irrigationsystem = NULL, landScen = NULL,
                             comAg = FALSE, aggregate = FALSE)
  fulfilledNAUww <- collapseNames(fulfilledNAU[, , "currHumanWWtotal"])
  fulfilledNAUwc <- collapseNames(fulfilledNAU[, , "currHumanWCtotal"])

  # Water Committed to Agriculture after Routing (in mio. m^3)
  fulfilledCAU   <- calcOutput("RiverHumanUseAccounting",
                               iteration = "committed_agriculture",
                               lpjml = lpjml, climatetype = climatetype,
                               efrMethod = efrMethod, transDist = transDist,
                               selectyears = selectyears, iniyear = iniyear,
                               multicropping = multicropping,
                               accessibilityrule = NULL,
                               rankmethod = NULL, gainthreshold = NULL,
                               cropmix = NULL, yieldcalib = NULL,
                               irrigationsystem = NULL, landScen = NULL,
                               comAg = FALSE, aggregate = FALSE)
  fulfilledCAUww <- collapseNames(fulfilledCAU[, , "currHumanWWtotal"])
  fulfilledCAUwc <- collapseNames(fulfilledCAU[, , "currHumanWCtotal"])

  ##############################
  ### Exogenous Water Demand ###
  ##############################
  # (would be required)
  actCAUww <- actCAUwc <- actNAUww <- actNAUwc <- new.magpie(fill = NA,
                                                             cells_and_regions = getItems(fulfilledCAUww, dim = 1),
                                                             years = getItems(fulfilledCAUww, dim = 2),
                                                             names = getItems(fulfilledCAUww, dim = 3))
  # Committed Agricultural Water (in mio. m^3)
  # multiple cropping as of current multiple cropping pattern
  if (as.logical(str_split(multicropping, ":")[[1]][1])) {
    m <- "TRUE:actual:irrig_crop"
  } else {
    m <- as.logical(str_split(multicropping, ":")[[1]][1])
  }
  actCAU   <- calcOutput("WaterUseCommittedAg",
                         lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, iniyear = iniyear,
                         multicropping = m, aggregate = FALSE)
  actCAUww[, , ] <- collapseNames(dimSums(actCAU[, , "withdrawal"], dim = 3))
  actCAUwc[, , ] <- collapseNames(dimSums(actCAU[, , "consumption"], dim = 3))

  # Non-Agricultural Water (in mio. m^3 / yr)
  watNonAg <- calcOutput("WaterUseNonAg",
                         selectyears = selectyears, cells = "lpjcell",
                         datasource = "WATERGAP_ISIMIP", usetype = "total",
                         seasonality = "total", harmonType = "average",
                         lpjml = NULL, climatetype = NULL, aggregate = FALSE)

  actNAUww[, , ] <- collapseNames(watNonAg[, , "withdrawal"])
  actNAUwc[, , ] <- collapseNames(watNonAg[, , "consumption"])

  ##########################
  ### Correct dimensions ###
  ##########################
  out <- new.magpie(fill = NA,
                    cells_and_regions = getCells(fulfilledCAUww),
                    years = getYears(fulfilledCAUww),
                    names = c("nonAg", "committedAg", "totalHuman"))
  # Transform object dimensions
  .transformObject <- function(x, gridcells, years, names) {
    # empty magpie object structure
    object0 <- new.magpie(cells_and_regions = gridcells,
                          years = years,
                          names = names,
                          fill = 0,
                          sets = c("x.y.iso", "year", "EFP.scen"))
    # bring object x to dimension of object0
    out <- object0 + x
    return(out)
  }

  out <- .transformObject(out,
                          gridcells = getItems(actNAUww, dim = 1),
                          years = getItems(actNAUww, dim = 2),
                          names = getItems(actNAUww, dim = 3))

  #################################
  ### Calculate Share Fulfilled ###
  #################################
  .calcShare <- function(fulfilledWW, actWW, fulfilledWC, actWC) {
    wwShr <- fulfilledWW / actWW
    wwShr[actWW == 0 & fulfilledWW == 0] <- NA
    wwShr[actWW == 0] <- NA
    wcShr <- fulfilledWC / actWC
    wcShr[actWC == 0 & fulfilledWC == 0] <- NA
    wcShr[actWC == 0] <- NA
    return(pmin(wcShr, wwShr))
  }
  out[, , "nonAg"] <- .calcShare(fulfilledWW = fulfilledNAUww,
                                 actWW = actNAUww,
                                 fulfilledWC = fulfilledNAUwc,
                                 actWC = actNAUwc)
  out[, , "committedAg"] <- .calcShare(fulfilledWW = fulfilledCAUww,
                                       actWW = actCAUww,
                                       fulfilledWC = fulfilledCAUwc,
                                       actWC = actCAUwc)
  out[, , "totalHuman"] <- .calcShare(fulfilledWW = fulfilledNAUww + fulfilledCAUww,
                                      actWW = actNAUww + actCAUww,
                                      fulfilledWC = fulfilledNAUwc + fulfilledCAUwc,
                                      actWC = actNAUwc + actCAUwc)

  ##############
  ### Checks ###
  ##############
  if (any(round(out, digits = 3) < 0, na.rm = TRUE)) {
    stop("calcShrHumanUsesFulfilled produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "fraction",
              description  = paste0("share of human water use ",
                                    "that cannot be fulfilled"),
              isocountries = FALSE))
}
