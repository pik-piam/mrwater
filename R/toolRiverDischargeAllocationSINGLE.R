#' @title       toolRiverDischargeAllocationSINGLE
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

toolRiverDischargeAllocationSINGLE <- function(rs, c,
                                               iteration, transDist,
                                               inLIST, inoutLIST) {

  # Inputs
  currReqWW <- inLIST$currReqWW
  currReqWC <- inLIST$currReqWC

  # Inputs that are also outputs
  # i.e. objects that are updated by this function)
  discharge      <- inoutLIST$discharge
  prevReservedWW <- inoutLIST$prevReservedWW

  # vector of downstreamcells of c
  vDOWN <- unlist(rs$downstreamcells[[c]])

  ##########################
  ###  Water Allocation  ###
  ##########################
  # Local water availability
  avlWatWW <- max(discharge[c] - prevReservedWW[c], 0)

  # Is water required for withdrawal in current grid cell?
  if (currReqWW > 0 && avlWatWW > 0) {
    ### Withdrawal Constraint:
    # Only as much can be withdrawn locally as is available
    fracFulfilled <- min(avlWatWW / currReqWW, 1)

    ### Consumption Constraint:
    # Cannot consume water locally that is required further downstream
    if (currReqWC > 0 && length(vDOWN) > 0) {

      # Water availability (considering downstream availability)
      avlWatWC <- max(min(discharge[vDOWN] - prevReservedWW[vDOWN]), 0)
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
  discharge[c(c, vDOWN)] <- discharge[c(c, vDOWN)] - currWClocal

  # update minimum water required in cell:
  prevReservedWW[c] <- prevReservedWW[c] + currWWlocal


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

    neighborCells <- unlist(rs$neighborcell[[c]])
    # Neighbor Irrigation (under "optimization" scenario)
    if ((transDist != 0) &&
        !is.null(neighborCells) &&
        (any(missingWW > 0) || any(missingWC > 0))) {

      # Water Allocation in neighboring cells of c
      # Loop over neighbor cells (by distance) until water requirements fulfilled
      for (n in neighborCells) {

        # If withdrawal constraint not fulfilled in neighbor cell:
        # jump directly to next neighbor
        if (discharge[n] - prevReservedWW[n] <= 0) {
          break
        }

        # Function inputs
        inLISTneighbor   <- list(currReqWW = missingWW,
                                 currReqWC = missingWC)
        inoutLISTneighbor <- list(discharge = discharge,
                                  prevReservedWW = prevReservedWW)

        # Neighbor Water Provision
        tmp <- toolRiverDischargeAllocationSINGLE(c = n, rs = rs,
                                            transDist = 0,
                                            iteration = "neighbor",
                                            inLIST = inLISTneighbor,
                                            inoutLIST = inoutLISTneighbor)
        discharge       <- tmp$discharge
        prevReservedWW  <- tmp$prevReservedWW

        # update reserved water in respective neighboring cell (current cell)
        fromNeighborWW <- fromNeighborWW + tmp$currWWlocal
        fromNeighborWC <- fromNeighborWC + tmp$currWClocal

        # Update locally missing water in c
        missingWW <- missingWW - tmp$currWWlocal
        missingWC <- missingWC - tmp$currWClocal

        # Checks
        if (round(missingWW, digits = 4) < 0 || round(missingWC, digits = 4) < 0) {
          stop(paste0("More water than necessary provided
                      in toolRiverDischargeAllocation by neighborcell ", n,
                      "to main cell ", c))
        }
        # Exit Neighbor Water Provision when enough water provided
        if (missingWW <= 0 && missingWC <= 0) {
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
