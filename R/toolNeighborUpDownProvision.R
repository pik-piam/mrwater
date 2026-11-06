#' @title       toolNeighborUpDownProvision
#' @description This function calculates water provision by
#'              surrounding grid cells for upstream-downstream
#'              allocation set-up
#'
#' @param rs             River structure including information
#'                       on neighboring cells
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param years          Vector of years for which neighbor allocation shall be applied
#' @param scenarios      Vector of scenarios for which neighbor allocation shall be applied
#' @param listNeighborIN List of arrays required for the algorithm:
#'                       yearlyRunoff, lakeEvap
#'                       reserved flows from previous water allocation round
#'                       (prevReservedWC, prevReservedWW)
#'                       missing water at this stage of water allocation
#'                       (missingWW, missingWC)
#'                       discharge
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @examples
#' \dontrun{
#' calcOutput("RiverHumanUseAccounting", aggregate = FALSE)
#' }
#'
#' @export

toolNeighborUpDownProvision <- function(rs, transDist,
                                        years, scenarios,
                                        listNeighborIN) {

  # read-in inputs
  prevWW    <- listNeighborIN$prevReservedWW
  prevWC    <- listNeighborIN$prevReservedWC
  missWW    <- listNeighborIN$missingWW
  missWC    <- listNeighborIN$missingWC
  discharge <- listNeighborIN$discharge
  inaccD    <- listNeighborIN$inaccD

  l            <- length(rs$cells)
  maxNeighbors <- max(lengths(rs$neighborcell))

  # Store sorted neighbor lists in a rectangular matrix so one neighbor rank can
  # be selected for many requesting cells at once inside the allocation rounds.
  # Rows are main cells; columns are neighbor cells, ranked by transport distance.
  neighborCellMatrix <- matrix(NA, nrow = l, ncol = maxNeighbors)
  if (maxNeighbors > 0) {
    for (cell in seq_len(l)) {
      neighbors <- rs$neighborcell[[cell]]
      if (!is.null(neighbors) && length(neighbors) > 0) {
        neighborCellMatrix[cell, seq_along(neighbors)] <- as.integer(neighbors)
      }
    }
  }

  # Pre-compute upstream-/downstream-cell subsets per cell.
  # River topology is invariant across years, scenarios and neighbor rounds,
  # so these subsets are built once here before the allocation loops.
  cellsRequestList   <- lapply(seq_len(l),
                               function(cell) as.integer(c(cell,
                                                           rs$upstreamcells[[cell]])))
  cellsDischargeList <- lapply(seq_len(l),
                               function(cell) as.integer(c(cell,
                                                           rs$downstreamcells[[cell]])))
  directUpstreamList <- vector("list", l)
  for (cell in seq_len(l)) {
    nextCell <- rs$nextcell[cell]
    if (nextCell > 0) {
      directUpstreamList[[nextCell]] <- c(directUpstreamList[[nextCell]], cell)
    }
  }
  directUpstreamCell <- integer(l)
  for (cell in seq_len(l)) {
    nUpstream <- length(directUpstreamList[[cell]])
    if (nUpstream == 1) {
      directUpstreamCell[cell] <- directUpstreamList[[cell]]
    } else if (nUpstream > 1) {
      directUpstreamCell[cell] <- -1
    }
  }
  directUpstreamList <- lapply(directUpstreamList,
                               function(upstreamCells) {
                                 upstreamCells[order(rs$calcorder[upstreamCells],
                                                     decreasing = FALSE)]
                               })

  # initialize objects
  fracFulfilled <- missWW
  fracFulfilled[, , ] <- NA
  toNeighborWW <- toNeighborWC <- fracFulfilled
  fromNeighborWW <- fromNeighborWC <- fracFulfilled

  ######################################
  ### Internal Function for neighbor ###
  ### cell water allocation          ###
  ######################################
  # assign fulfilled water to neighbor cell that requested water
  .assignToMain <- function(requestingCellsByGivingCell,
                            cellsGiving,
                            missing, toNeighbor) {
    fracAssigned <- numeric(length(missing))
    # `cellsGiving` contains the neighbor-cell indices that received a request
    # in this round. Only some of them can actually fulfill requested water after
    # upstream/downstream accounting (cellsGivingFulfilled).
    cellsGivingFulfilled <- toNeighbor[cellsGiving] > 0
    cellsGiving <- cellsGiving[cellsGivingFulfilled]
    # list of requesting cells by giving cell:
    # each list element contains the cells requesting water from one giving neighbor cell.
    # The list names are the giving-cell indices, and the list values are the requesting-cell indices
    requestingCellsByGivingCell <- requestingCellsByGivingCell[cellsGivingFulfilled]

    # Loop through neighbor cells
    for (i in seq_along(cellsGiving)) {
      # `i` is the position in the grouped list of giving neighbor cells
      # `s` is the actual giving-cell index used for vector lookups
      s <- cellsGiving[i]
      cellReceiving <- requestingCellsByGivingCell[[i]]
      # Missing consumptive water in the main cells that requested water from
      # giving neighbor cell s. These values determine each receiver's share.
      missReceiving <- missing[cellReceiving]
      shr           <- missReceiving / sum(missReceiving)
      # volume assigned to neighbor
      fromNeighbor <- shr * toNeighbor[s]

      # Only cells with positive missing water can receive a non-zero
      # fulfillment fraction; zero-missing cells stay at the initialized 0.
      positiveReceiving <- missReceiving > 0
      if (any(positiveReceiving)) {
        fracAssigned[cellReceiving[positiveReceiving]] <-
          (fromNeighbor / missReceiving)[positiveReceiving]
      }
    }
    return(fracAssigned)
  }

  #####################################
  ### Neighbor Allocation Algorithm ###
  #####################################
  for (y in years) {
    for (scen in scenarios) {

      # initialize objects
      tmpDischarge <- discharge[, y, scen]
      tmpMissWW <- missWW[, y, scen]
      tmpMissWC <- missWC[, y, scen]
      tmpPrevWW <- prevWW[, y, scen]
      tmpPrevWC <- prevWC[, y, scen]
      tmpRunoffWOEvap <- listNeighborIN$runoffWOEvap[, y, scen]

      ###################################################
      ### Iterations of Neighbor Cell Water Provision ###
      ###################################################
      fromWW <- fromWC <- toWW <- toWC <- numeric(l)
      # Loop through all neighboring cells within
      # certain distance sorted by distance
      for (i in seq_len(maxNeighbors)) {

        # initialize requested water
        tmpRequestWWlocal <- numeric(l)
        tmpRequestWClocal <- numeric(l)

        # exclude cells with insufficient available water
        # (recomputed every neighbor round: depends on the current
        #  discharge and previously reserved withdrawal of this year/scenario)
        flag <- which(tmpDischarge < tmpPrevWW)
        flaggedEmpty <- logical(l)
        if (length(flag) > 0) {
          flaggedEmpty[unlist(cellsRequestList[flag], use.names = FALSE)] <- TRUE
        }

        cellsRequestingNeighborWater <- which(tmpMissWW > 0)

        # Select the next usable neighbor for each requesting main cell in this
        # neighbor-distance round. The mapping is first represented per
        # requesting cell, then grouped below by the selected giving cell.
        requestingCellsByGivingCell <- list()
        cellsGiving <- integer(0)
        if (length(cellsRequestingNeighborWater) > 0) {
          neighborPosition <- rep.int(i, length(cellsRequestingNeighborWater))
          selectedNeighbor <- neighborCellMatrix[cbind(cellsRequestingNeighborWater,
                                                       neighborPosition)]

          # If the selected neighbor has insufficient available water, move only
          # those requesting cells to their next neighbor rank until a usable
          # neighbor is found or the neighbor list is exhausted.
          validNeighbor <- !is.na(selectedNeighbor)
          while (any(validNeighbor & flaggedEmpty[selectedNeighbor])) {
            skipped <- validNeighbor & flaggedEmpty[selectedNeighbor]
            neighborPosition[skipped] <- neighborPosition[skipped] + 1
            skippedIndex <- which(skipped)
            validPosition <- neighborPosition[skippedIndex] <= maxNeighbors
            selectedNeighbor[skippedIndex] <- NA
            if (any(validPosition)) {
              updateIndex <- skippedIndex[validPosition]
              updateRows <- cbind(cellsRequestingNeighborWater[updateIndex],
                                  neighborPosition[updateIndex])
              selectedNeighbor[updateIndex] <- neighborCellMatrix[updateRows]
            }
            validNeighbor <- !is.na(selectedNeighbor)
          }

          requestingCells <- cellsRequestingNeighborWater[validNeighbor]
          givingCellsByRequest <- selectedNeighbor[validNeighbor]
          if (length(givingCellsByRequest) > 0) {

            # Sum all requests by giving neighbor cell. `givingCellsByRequest`
            # has one entry per requesting cell, while `givingCellsUnique` has
            # one entry per neighbor cell that receives a request in this round.
            requestByGiving <- rowsum(cbind(tmpMissWW[requestingCells],
                                            tmpMissWC[requestingCells]),
                                      givingCellsByRequest,
                                      reorder = FALSE)
            givingCellsUnique <- as.integer(rownames(requestByGiving))
            tmpRequestWWlocal[givingCellsUnique] <- requestByGiving[, 1]
            tmpRequestWClocal[givingCellsUnique] <- requestByGiving[, 2]
            # Reverse map from giving neighbor cell to all main cells that
            # requested water from it. Names are actual giving-cell indices;
            # list positions are aligned with `cellsGiving`.
            requestingCellsByGivingCell <-
              split(requestingCells, givingCellsByRequest)
            cellsGiving <- as.integer(names(requestingCellsByGivingCell))
          }
        }
        # Total water requested in this round of neighbor water provision
        tmpRequestWCtotal <- tmpRequestWClocal
        tmpRequestWWtotal <- tmpRequestWWlocal

        # Select cells to be calculated
        cellsCalc <- unique(c(which(tmpRequestWWlocal > 0),
                              which(tmpDischarge + tmpPrevWC < tmpPrevWW)))
        cellsCalc <- unique(c(cellsCalc, unlist(rs$downstreamcells[cellsCalc])))
        cellsCalc <- cellsCalc[order(rs$calcorder[cellsCalc], decreasing = FALSE)]

        dischargeBeforeNeighborRound <- tmpDischarge

        # Repeat Upstream-Downstream Reservation for
        # neighboring cells
        for (c in cellsCalc) {

          if ((tmpRequestWWlocal[c] > 0) ||
              ((tmpDischarge[c] + tmpPrevWC[c]) < tmpPrevWW[c])) {
            # Look up pre-computed upstream-/downstream-cell subsets
            cellsRequest   <- cellsRequestList[[c]]
            cellsDischarge <- cellsDischargeList[[c]]

            # Reserved Water Use Accounting
            tmp <- toolRiverUpDownBalance(inLIST = list(prevWC = tmpPrevWC[c],
                                                        prevWW = tmpPrevWW[c],
                                                        currWW = tmpRequestWWlocal[c],
                                                        inaccD = inaccD[c]),
                                          inoutLIST = list(disc = tmpDischarge[cellsDischarge],
                                                           currWC = tmpRequestWClocal[cellsRequest]))

            # Updated flows
            tmpDischarge[cellsDischarge]    <- tmp$disc
            tmpRequestWClocal[cellsRequest] <- tmp$currWC
          }
        }
        # Water reserved in this round is reserved as previous use
        # before next round of neighbor water provision
        tmpPrevWC <- tmpPrevWC + tmpRequestWClocal
        # Convert the possibly reduced consumptive request into a fulfillment
        # fraction relative to the original request of this neighbor round.
        fracFulfilled <- numeric(l)
        requestedWC <- tmpRequestWCtotal > 0
        fracFulfilled[requestedWC] <-
          (tmpRequestWClocal / tmpRequestWCtotal)[requestedWC]
        tmpRequestWWlocal <- fracFulfilled * tmpRequestWWtotal
        tmpPrevWW <- tmpPrevWW + tmpRequestWWlocal

        # Update only cells affected by additional consumptive use in this
        # neighbor round. The discharge effect of a consumption change propagates
        # from the giving cell through all downstream cells.
        cellsDischargeUpdate <- unique(unlist(cellsDischargeList[which(tmpRequestWClocal != 0)],
                                              use.names = FALSE))
        cellsDischargeUpdate <- cellsDischargeUpdate[order(rs$calcorder[cellsDischargeUpdate],
                                                           decreasing = FALSE)]
        tmpDischarge <- toolRiverDischargeUpdateAffectedCells(runoffWOEvap = tmpRunoffWOEvap,
                                                              watCons = tmpPrevWC,
                                                              cellsCalc = cellsDischargeUpdate,
                                                              previousDischarge = dischargeBeforeNeighborRound,
                                                              directUpstreamList = directUpstreamList,
                                                              directUpstreamCell = directUpstreamCell)

        # Assign reserved flows to cell that had requested the water
        fracFromNeighbor <- .assignToMain(requestingCellsByGivingCell =
                                            requestingCellsByGivingCell,
                                          cellsGiving = cellsGiving,
                                          missing = tmpMissWC,
                                          toNeighbor = tmpRequestWClocal)

        # Water provided from neighbor
        fromWW <- fromWW + tmpMissWW * fracFromNeighbor
        fromWC <- fromWC + tmpMissWC * fracFromNeighbor

        # Water provided to neighbor
        toWC <- toWC + tmpRequestWClocal
        toWW <- toWW + tmpRequestWWlocal

        # Update water that is still missing in main river cell(s)
        tmpMissWW <- tmpMissWW * (1 - fracFromNeighbor)
        tmpMissWC <- tmpMissWC * (1 - fracFromNeighbor)

        # repeat until no more missing water OR no more neighbor cells
        if (all(tmpMissWW <= 0) && all(tmpMissWC <= 0)) {
          break
        }
      }

      # Save result for respective scenario
      discharge[, y, scen]      <- tmpDischarge
      toNeighborWC[, y, scen]   <- toWC
      toNeighborWW[, y, scen]   <- toWW
      missWW[, y, scen]         <- tmpMissWW
      missWC[, y, scen]         <- tmpMissWC
      fromNeighborWW[, y, scen] <- fromWW
      fromNeighborWC[, y, scen] <- fromWC
    }
  }

  # Return output
  out <- list(discharge = discharge,
              missingWW = missWW,
              missingWC = missWC,
              toNeighborWW = toNeighborWW,
              toNeighborWC = toNeighborWC,
              fromNeighborWW = fromNeighborWW,
              fromNeighborWC = fromNeighborWC)

  return(out)
}
