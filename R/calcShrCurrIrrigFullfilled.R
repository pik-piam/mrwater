#' @title       calcShrCurrIrrigFullfilled
#' @description calculates of share of current irrigation that can be fulfilled
#'              given renewable water availability of the algorithm
#'
#' @param lpjml            LPJmL version used
#' @param climatetype      Switch between different climate scenarios or
#'                         historical baseline "GSWP3-W5E5:historical"
#' @param selectyears      Years to be returned
#' @param iniyear          Initialization year
#' @param EFRmethod        EFR method used including selected strictness of EFRs
#'                         (e.g. Smakhtin:good, VMF:fair)
#'
#' @return cellular magpie object
#' @author Felicitas Beier
#'
#' @examples
#' \dontrun{
#' calcShrCurrIrrigFullfilled()
#' }
#'
#' @importFrom magclass collapseNames dimSums
#'
#' @export

calcShrCurrIrrigFullfilled <- function(lpjml, climatetype,
                                       selectyears, iniyear, EFRmethod) {

  ### Reasons for not-fulfilled actually observed irrigation:
  # - fossil groundwater is used for irrigation (e.g. Northern India), but not accounted for in the river routing
  # - long-distance water diversions take place (e.g. Northern China), but not accounted for in the river routing
  # - deficit irrigation is in place (e.g. Southern Spain), but not accounted for in the river routing
  # - water reuse is not accounted for in the river routing

  # Committed Agricultural Water (in mio. m^3)
  actCAU   <- calcOutput("WaterUseCommittedAg", lpjml = lpjml, climatetype = climatetype,
                         selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)
  actCAUww <- collapseNames(dimSums(actCAU[, , "withdrawal"], dim = 3))
  actCAUwc <- collapseNames(dimSums(actCAU[, , "consumption"], dim = 3))

  # Water Committed to Agriculture after Routing (in mio. m^3)
  fulfilledCAU   <- calcOutput("RiverHumanUses", humanuse = "committed_agriculture",
                               lpjml = lpjml, climatetype = climatetype, EFRmethod = EFRmethod,
                               selectyears = selectyears, iniyear = iniyear, aggregate = FALSE)
  fulfilledCAUww <- collapseNames(fulfilledCAU[, , "currHuman_ww"])
  fulfilledCAUwc <- collapseNames(fulfilledCAU[, , "currHuman_wc"])

  wwShr <- fulfilledCAUww / actCAUww
  wwShr[actCAUww == 0 & fulfilledCAUww == 0] <- 0
  wwShr[actCAUww == 0] <- NA
  wcShr <- fulfilledCAUwc / actCAUwc
  wcShr[actCAUwc == 0 & fulfilledCAUwc == 0] <- 0
  wcShr[actCAUwc == 0] <- NA

  fulfilledShr <- pmin(wcShr, wwShr)

  return(list(x            = fulfilledShr,
              weight       = NULL,
              unit         = "fraction",
              description  = "shared of committed agricultural use
                              that cannot be fulfilled",
              isocountries = FALSE))
}
