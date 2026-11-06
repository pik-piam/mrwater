#' @title       toolRiverDischargeUpdateAffectedCells
#' @description This function calculates cellular discharge
#'              after reserving water uses for consumption
#'              only for affected cells after a local
#'              consumptive-use update
#'
#' @param runoffWOEvap       Array that contains (runoff - lake evap)
#' @param watCons            Array that contains water reserved for consumptive
#'                           use
#' @param cellsCalc          Integer vector of affected cells in upstream-to-
#'                           downstream order
#' @param previousDischarge  Discharge vector before the current update
#' @param directUpstreamList List of direct upstream cells per cell
#' @param directUpstreamCell Integer vector with the only direct upstream cell
#'                           per cell; 0 means no upstream cell and -1 means
#'                           multiple upstream cells
#'
#' @return array in cellular resolution

toolRiverDischargeUpdateAffectedCells <- function(runoffWOEvap, watCons,
                                                  cellsCalc,
                                                  previousDischarge,
                                                  directUpstreamList,
                                                  directUpstreamCell) {

  discharge <- previousDischarge

  for (c in cellsCalc) {
    # select upstream cell of current cell
    upstreamCell <- directUpstreamCell[c]

    if (upstreamCell > 0) {
      # re-calculate inflow from upstream cell
      inflow <- discharge[upstreamCell]
    } else if (upstreamCell == 0) {
      inflow <- 0
    } else {
      inflow <- 0
      for (u in directUpstreamList[[c]]) {
        inflow <- inflow + discharge[u]
      }
    }
    # Update river discharge
    discharge[c] <- inflow + runoffWOEvap[c] - watCons[c]
  }

  if (length(cellsCalc) > 0) {
    dischargeCheck <- discharge[cellsCalc]
    if (anyNA(dischargeCheck)) {
      stop("toolRiverDischargeUpdateAffectedCells finished with NA discharge")
    }
    if (any(round(dischargeCheck, digits = 6) < 0)) {
      stop("toolRiverDischargeUpdateAffectedCells finished with negative values")
    }
  }

  return(discharge)
}
