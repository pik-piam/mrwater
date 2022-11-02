#' @title       toolRiverDischargeUpdate
#' @description This function calculates cellular discharge
#'              after reserving water uses for consumption
#'
#' @param rs            River structure
#' @param runoffWOEvap  Array that contains (runoff - lake evap)
#' @param watCons       Array that contains water reserved
#'                      for consumptive use
#'
#' @return array in cellular resolution and all year and scenario dimensions
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverHumanUseAccounting", aggregate = FALSE)
#' }
#'
toolRiverDischargeUpdate <- function(rs, runoffWOEvap, watCons) {

  # helper variables in correct dimension
  # (initialized to zero)
  inflow <- runoffWOEvap
  inflow[, , ] <- 0
  avlWat <- discharge <- inflow

  ###########################################
  ###### River Discharge Calculation ########
  ###########################################
  for (o in 1:max(rs$calcorder)) {

    # Note: the calcorder ensures that the upstreamcells are calculated first
    cells <- which(rs$calcorder == o)

    for (c in cells) {

      # available water
      avlWat[c, , ] <- inflow[c, , , drop = FALSE] + runoffWOEvap[c, , , drop = FALSE]

      # discharge
      discharge[c, , ] <- avlWat[c, , , drop = FALSE] - watCons[c, , , drop = FALSE]

      # inflow into nextcell
      if (rs$nextcell[c] > 0) {
        inflow[rs$nextcell[c], , ] <- inflow[rs$nextcell[c], , , drop = FALSE] + discharge[c, , , drop = FALSE]
      }
    }
  }

  # Check for NAs
  if (any(is.na(discharge))) {
    stop("toolRiverDischargeUpdate finished with NA discharge")
  }
  # Check for negative discharge
  if (any(round(discharge) < 0)) {
    stop("toolRiverDischargeUpdate finished with negative values")
  }

  return(discharge)
}
