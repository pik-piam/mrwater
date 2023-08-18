#' @title       toolRiverDischargeAllocation
#' @description This tool function allocates discharge
#'              for grid cells respecting upstream-downstream relationships
#'              and various water constraints
#'
#' @param iteration      Currently active iteration of river discharge allocation.
#'                       Arguments:
#'                       "main" for case of main river cells
#'                       "neighbor" for case of neighboring cells of main river cells
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param c              Current cell for which water shall be allocated
#' @param downCells      Downstream cells of c
#' @param rs             River structure with information on upstreamcells,
#'                       downstreamcells and neighboring cells and distances
#' @param inLIST         List of objects that are inputs to the function
#'                       irrigGain, gainthreshold,
#' @param inoutLIST      List of objects that are inputs to the function and
#'                       are updated by the function
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolRiverDischargeAllocation <- function(rs, c,
                                        downCells,
                                        iteration, transDist,
                                        inLIST, inoutLIST) {
  # Inputs
  currReqWW <- inLIST$currReqWW
  currReqWC <- inLIST$currReqWC

  # Inputs that are also outputs
  # i.e. objects that are updated by this function)
  discharge      <- inoutLIST$discharge[drop = FALSE]
  prevReservedWW <- inoutLIST$prevReservedWW[drop = FALSE]

  # Selected cells
  if (length(discharge) == 1) {
    cell <- 1
  } else {
    cell <- rs$isoCoord[c]
  }
  if (length(downCells) > 0) {
    downCells <- rs$isoCoord[downCells]
  }
  allCells <- c(cell, downCells)

  ##########################
  ###  Water Allocation  ###
  ##########################
  # Local water availability
  avlWatWW <- max(discharge[cell] - prevReservedWW[cell], 0)

  # Is water required for withdrawal in current grid cell?
  if (currReqWW > 0 && avlWatWW > 0) {
    ### Withdrawal Constraint:
    # Only as much can be withdrawn locally as is available
    fracFulfilled <- min(avlWatWW / currReqWW, 1)

    ### Consumption Constraint:
    # Cannot consume water locally that is required further downstream
    if (currReqWC > 0 && length(downCells) > 0) {

      # Water availability (considering downstream availability)
      avlWatWC <- max(min(discharge[downCells] - prevReservedWW[downCells]), 0)
      # Update fraction of irrigation water requirements that can be fulfilled
      fracFulfilled <- min(avlWatWC / currReqWC, fracFulfilled)
    }
  } else {
    # If no water requested: fracFulfilled not relevant
    fracFulfilled <- 0
  }

  # store locally fulfilled water withdrawals and consumption
  currWClocal <- currReqWC * fracFulfilled
  currWWlocal <- currReqWW * fracFulfilled

  # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
  discharge[allCells]  <- discharge[allCells] - currWClocal

  # update minimum water required in cell:
  prevReservedWW[cell] <- prevReservedWW[cell] + currWWlocal


  # Initialize objects
  # to be filled (in case of main iteration)
  # remain zero (in case of neighbor iteration)
  fromNeighborWC <- fromNeighborWW <- 0

  # The following calculations are only relevant
  # if the current cell c is a cell of the main river,
  # i.e. water can be requested from neighboring cells
  if (iteration == "main") {

    # Locally missing water that might be fulfilled by surrounding cells
    missingWW <- currReqWW - currWWlocal
    missingWC <- currReqWC - currWClocal

    neighborsOfC <- rs$neighborcell[[c]]
    # Neighbor Irrigation (under "optimization" scenario)
    if ((transDist != 0) &&
        !is.null(neighborsOfC) &&
        (missingWW > 1e-4 || missingWC > 1e-4)) {

      # Water Allocation in neighboring cells of c
      # Loop over neighbor cells (by distance) until water requirements fulfilled
      for (n in neighborsOfC) {

        names(n) <- rs$isoCoord[n]
        # If withdrawal constraint not fulfilled in neighbor cell:
        # jump directly to next neighbor
        if (discharge[names(n)] - prevReservedWW[names(n)] <= 0) {
          break
        }
        # Select relevant cells
        selectCells        <- c(n, rs$downstreamcells[[n]])
        names(selectCells) <- rs$isoCoord[selectCells]

        # Function inputs
        inLISTneighbor    <- list(currReqWW = missingWW,
                                  currReqWC = missingWC)
        inoutLISTneighbor <- list(discharge = discharge[names(selectCells)],
                                  prevReservedWW = prevReservedWW[names(selectCells)])

        # Neighbor Water Provision
        tmp <- toolRiverDischargeAllocation(c = n, rs = rs,
                                            downCells = selectCells[-1],
                                            transDist = 0,
                                            iteration = "neighbor",
                                            inLIST = inLISTneighbor,
                                            inoutLIST = inoutLISTneighbor)
        discharge[names(selectCells)]       <- tmp$discharge
        prevReservedWW[names(selectCells)]  <- tmp$prevReservedWW

        # update reserved water in respective neighboring cell (current cell)
        fromNeighborWW <- fromNeighborWW + tmp$currWWlocal
        fromNeighborWC <- fromNeighborWC + tmp$currWClocal

        # Update locally missing water in c
        missingWW <- missingWW - tmp$currWWlocal
        missingWC <- missingWC - tmp$currWClocal

        # Checks
        if (round(missingWW, digits = 4) < 0) {
          stop(paste0("More water than necessary provided ",
                      "in toolRiverDischargeAllocation by neighborcell ", n,
                      "to main cell ", c))
        }
        # Exit Neighbor Water Provision when enough water provided
        if (missingWW <= 1e-4 && missingWC <= 1e-4) {
          break
        }
      }
    }
  }

  out <- list(discharge = discharge,
              prevReservedWW = prevReservedWW,
              fromNeighborWC = fromNeighborWC,
              fromNeighborWW = fromNeighborWW,
              currWWlocal = currWWlocal,
              currWClocal = currWClocal)
  return(out)
}
