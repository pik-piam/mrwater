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
#'                      ("endogenous": suitability for multiple cropping determined
#'                                    by rules based on grass and crop productivity
#'                      "exogenous": suitability for multiple cropping given by
#'                                   GAEZ data set),
#'                      separated by ":"
#'                      (e.g. TRUE:endogenous; TRUE:exogenous; FALSE)
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
  # - water reuse is not accounted for in the river routing

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
                                                             cells_and_regions = getCells(fulfilledCAUww),
                                                             years = getYears(fulfilledCAUww),
                                                             names = getNames(fulfilledCAUww))
  # Committed Agricultural Water (in mio. m^3)
  actCAU   <- calcOutput("WaterUseCommittedAg",
                         lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, iniyear = iniyear,
                         multicropping = multicropping, aggregate = FALSE)
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
  .transformObject <- function(x) {
      # empty magpie object structure
      object0 <- new.magpie(cells_and_regions = getItems(actNAUww, dim = 1),
                            years = getItems(actNAUww, dim = 2),
                            names = getItems(actNAUww, dim = 3),
                            fill = 0,
                            sets = c("x.y.iso", "year", "EFP.scen"))
      # bring object x to dimension of object0
      out <- object0 + x
      return(out)
  }
  out <- .transformObject(out)

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
  if (any(round(out) < 0, na.rm = TRUE)) {
    stop("calcShrHumanUsesFulfilled produced negative values")
  }

  return(list(x            = out,
              weight       = NULL,
              unit         = "fraction",
              description  = "share of human water use
                              that cannot be fulfilled",
              isocountries = FALSE))
}