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

toolRiverDischargeUpdate <- function(rs, runoffWOEvap, watCons) {

  # helper variables in correct dimension
  # (initialized to zero)
  inflow <- avlWat <- discharge <- numeric(length(runoffWOEvap))

  ###########################################
  ###### River Discharge Calculation ########
  ###########################################
  # All grid cells in the river network
  cellsCalc <- seq_along(runoffWOEvap)
  # Ordering grid cells from upstream to downstream
  cellsCalc <- cellsCalc[order(rs$calcorder[cellsCalc], decreasing = FALSE)]

  for (c in cellsCalc) {

      # available water in cell
      avlWat[c] <- inflow[c] + runoffWOEvap[c]

      # discharge out of cell
      discharge[c] <- avlWat[c] - watCons[c]

        # inflow into nextcell
        if (rs$nextcell[c] > 0) {
          inflow[rs$nextcell[c]] <- inflow[rs$nextcell[c]] + discharge[c]
      }
  }

  # Check for NAs
  if (any(is.na(discharge))) {
    stop("toolRiverDischargeUpdate finished with NA discharge")
  }
  # Check for negative discharge
  if (any(round(discharge, digits = 6) < 0)) {
    stop("toolRiverDischargeUpdate finished with negative values")
  }

  return(discharge)
}
