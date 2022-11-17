#' @title       toolRiverDischargeAllocation
#' @description This tool function allocates discharge
#'              for grid cells respecting upstream-downstream relationships
#'              and various water constraints
#'
#' @param c              Current cell for which water shall be allocated
#' @param rs             River structure with information on upstreamcells,
#'                       downstreamcells and neighboring cells and distances
#' @param iteration      Currently active iteration of river discharge allocation.
#'                       Arguments:
#'                       "main" for case of main river cells
#'                       "neighbor" for case of neighboring cells of main river cells
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param inLIST         List of objects that are inputs to the function
#'                       irrigGain, gainthreshold,
#' @param inoutLIST      List of objects that are inputs to the function and
#'                       are updated by the function
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolRiverDischargeAllocation <- function(c, rs, transDist,
                                         iteration, inLIST, inoutLIST) {

  # Helper vectors for subsetting of objects
  # vector of downstreamcells of c
  vDOWN <- unlist(rs$downstreamcells[[c]])
  # vector of c in length of downstreamcells of c
  vCELL <- rep(c, length(rs$downstreamcells[[c]]))
  # vector of 1s in length of downstreamcells of c
  vONES <- rep(1, length(rs$downstreamcells[[c]]))

  ###############################
  ### Import required objects ###
  ###############################
  # Inputs
  currReqWW <- inLIST$currReqWW
  currReqWC <- inLIST$currReqWC

  # Inputs that are also outputs
  # i.e. objects that are updated by this function)
  discharge      <- inoutLIST$discharge
  prevReservedWW <- inoutLIST$prevReservedWW
  currWWlocal    <- inoutLIST$currWWlocal
  currWClocal    <- inoutLIST$currWClocal

  # initialize objects to be filled
  fracFulfilled <- discharge
  fracFulfilled[, , ] <- 0
  avlWatWW <- avlWatWC <- fracFulfilled

  ##################
  ### Conditions ###
  ##################
  # is water required for withdrawal in current grid cell?
  isWW <- (currReqWW[, , , drop = FALSE] > 0)
  # is water required for consumption in current grid cell?
  isWC <- (currReqWC[c, , , drop = FALSE] > 0 & isWW)

  ##########################
  ### Internal Functions ###
  ##########################
  .determineAvailability <- function(dis, res) {
    # available water for additional irrigation withdrawals
    return(pmax(apply(dis - res, MARGIN = 3, min), 0))
  }
  .determineFractionFulfilled <- function(avl, req, frac) {
    # how much withdrawals can be fulfilled by available water
    return(pmin(avl / req, frac))
  }
  .updateUse <- function(wat, frac) {
    return(wat * frac)
  }
  .updateReserved <- function(curUse, res) {
    return(res + curUse)
  }
  .subtractFlows <- function(a, b) {
    return(a - b)
  }

  ##########################
  ###  Water Allocation  ###
  ##########################

  # Water Availability
  avlWatWW[c, , ] <- .determineAvailability(dis = discharge[c, , , drop = FALSE],
                                            res = prevReservedWW[c, , , drop = FALSE])

  # Withdrawal Constraint
  fracFulfilled[c, , ][isWW] <- .determineFractionFulfilled(avl = avlWatWW[c, , , drop = FALSE][isWW],
                                                            req = currReqWW[c, , , drop = FALSE][isWW],
                                                            frac = 1)

  # Account for downstream availability
  if (length(vDOWN) > 0) {

    # Water Availability (considering downstream availability)
    avlWatWC[c, , ][isWC] <- .determineAvailability(dis = discharge[vDOWN, , , drop = FALSE],
                                                     res = prevReservedWW[vDOWN, , , drop = FALSE])[isWC]

    # Consumption Constraint
    fracFulfilled[c, , ][isWC] <- .determineFractionFulfilled(avl = avlWatWC[c, , , drop = FALSE][isWC],
                                                         req = currReqWC[c, , , drop = FALSE][isWC],
                                                         frac = fracFulfilled[c, , , drop = FALSE][isWC])
  }

  # locally fulfilled water withdrawals and consumption
  currWClocal[c, , ] <- .updateUse(wat  = currReqWC[c, , , drop = FALSE],
                                    frac = fracFulfilled[c, , , drop = FALSE])
  currWWlocal[c, , ] <- .updateUse(wat  = currReqWW[c, , , drop = FALSE],
                                    frac = fracFulfilled[c, , , drop = FALSE])

  # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
  discharge[c(vDOWN, c), , ][isWW[c(vONES, 1), , ,
                                 drop = FALSE]] <- .subtractFlows(a = discharge[c(vDOWN, c), , , drop = FALSE],
                                                                  b = currWClocal[c(vCELL, c), , , drop = FALSE]
                                                                  )[isWW[c(vONES, 1), , , drop = FALSE]]
  # update minimum water required in cell:
  prevReservedWW[c, , ][isWW] <- .updateReserved(res = prevReservedWW[c, , , drop = FALSE],
                                                 curUse = currWWlocal[c, , , drop = FALSE])[isWW]


  # The following calculations are only relevant
  # if the current cell c is a cell of the main river,
  # i.e. water can be requested from neighboring cells
  if (iteration == "main") {

    # Inputs that are also outputs,
    # but only relevant for neighbor water provision
    # i.e. objects that are updated by this function
    fromNeighborWW <- inoutLIST$fromNeighborWW
    fromNeighborWC <- inoutLIST$fromNeighborWC
    toNeighborWW   <- inoutLIST$toNeighborWW
    toNeighborWC   <- inoutLIST$toNeighborWC

    # Locally missing water that might be fulfilled by surrounding cells
    missingWW[c, , ] <- .subtractFlows(a = currReqWW[c, , , drop = FALSE],
                                       b = currWWlocal[c, , , drop = FALSE])
    missingWC[c, , ] <- .subtractFlows(a = currReqWC[c, , , drop = FALSE],
                                       b = currWClocal[c, , , drop = FALSE])

    # Neighbor Irrigation (under "optimization" scenario)
    if ((transDist != 0) &&
        !is.null(rs$neighborcell[[c]]) &&
        (any(missingWW[c, , ] != 0) || any(missingWC[c, , ] != 0))) {

      neighborWWlocal <- neighborWClocal <- discharge
      neighborWWlocal[, , ] <- 0
      neighborWClocal[, , ] <- 0

      # Initialize objects to be filled
      tmp <- discharge
      tmp[, , ] <- 0
      fromNeighborWC <- fromNeighborWW <- toNeighborWC <- toNeighborWW <- tmp

      # Water Allocation in neighboring cells of c
      # Loop over neighbor cells (by distance) until water requirements fulfilled
      for (n in rs$neighborcell[[c]]) {
        inLISTneighbor <- list(currReqWW = missingWW,
                               currReqWC = missingWC)

        inoutLISTneighbor <- list(discharge = discharge,
                                  reserved = prevReservedWW,

                                  currWWlocal = neighborWWlocal,
                                  currWClocal = neighborWClocal)

        tmp <- toolRiverDischargeAllocation(c = n, rs = rs,
                                            iteration = "neighbor",
                                            inLIST = inLISTneighbor,
                                            inoutLIST = inoutLISTneighbor)
        discharge       <- tmp$discharge
        prevReservedWW  <- tmp$prevReservedWW
        neighborWWlocal <- tmp$currWWlocal
        neighborWClocal <- tmp$currWClocal

        # update reserved water in respective neighboring cell (current cell)
        fromNeighborWW[c, , ] <- .updateReserved(res = fromNeighborWW[c, , , drop = FALSE],
                                                curUse = tmp$currWWlocal[n, , , drop = FALSE])
        fromNeighborWC[c, , ] <- .updateReserved(res = fromNeighborWC[c, , , drop = FALSE],
                                                curUse = tmp$currWClocal[n, , , drop = FALSE])

        # update water provided by neighboring cell to original cell that had missing water
        toNeighborWW[n, , ] <- .updateReserved(res = toNeighborWW[n, , , drop = FALSE],
                                                      curUse = tmp$currWWlocal[n, , , drop = FALSE])
        toNeighborWC[n, , ] <- .updateReserved(res = toNeighborWC[n, , , drop = FALSE],
                                                      curUse = tmp$currWClocal[n, , , drop = FALSE])

        # Update locally missing water in c
        missingWW[c, , ] <- missingWW[c, , , drop = FALSE] - tmp$currWWlocal[n, , , drop = FALSE]
        missingWC[c, , ] <- missingWC[c, , , drop = FALSE] - tmp$currWClocal[n, , , drop = FALSE]

        # Checks
        if (any(missingWW < 0)) {
          stop(paste0("More water than necessary provided to missingWW
                      in toolRiverDischargeAllocation by neighborcell ", c))
        }
        if (any(missingWC < 0)) {
          stop(paste0("More water than necessary provided to missingWC
                      in toolRiverDischargeAllocation by neighborcell ", c))
        }
        # Exit Neighbor Water Provision when enough water provided
        if (all(missingWW <= 0) && all(missingWC <= 0)) {
          break
        }
      }
    } else {
      stop("In toolRiverDischargeAllocation:
            Please select `main` or `neighbor` in `iteration` argument
            to indicate stage of discharge allocation.")
    }
  }

  out <- list(discharge = discharge,
              prevReservedWW = prevReservedWW,
              fromNeighborWC = fromNeighborWC,
              fromNeighborWW = fromNeighborWW,
              toNeighborWW = toNeighborWW,
              toNeighborWC = toNeighborWC,
              currWWlocal = currWWlocal,
              currWClocal = currWClocal)
  return(out)
}
