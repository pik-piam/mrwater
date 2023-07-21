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
  prevWW <- listNeighborIN$prevReservedWW
  prevWC <- listNeighborIN$prevReservedWC
  missWW <- listNeighborIN$missingWW
  missWC <- listNeighborIN$missingWC
  discharge <- listNeighborIN$discharge

  l <- length(rs$cells)
  names(rs$neighborcell) <- 1:l  ## check whether still necessary
  names(rs$neighbordist) <- 1:l  ## check whether still necessary

  # initialize objects
  fracFulfilled <- missWW
  fracFulfilled[, , ] <- NA
  toNeighborWW <- toNeighborWC <- fracFulfilled
  fromNeighborWW <- fromNeighborWC <- fracFulfilled

  ### Internal Function ###
  # assign fulfilled water to neighbor cell that requested water
  .assignToMain <- function(requestingList,
                            missing, toNeighbor) {
    fracAssigned <- numeric(length(missing))
    cellsGiving  <- which(toNeighbor > 0)

    for (s in cellsGiving) {
      cellReceiving <- requestingList[[s]]
      shr           <- missing[cellReceiving] / sum(missing[cellReceiving])
      # volume assigned to neighbor
      fromNeighbor <- shr * toNeighbor[s]

      fracAssigned[cellReceiving] <- ifelse(missing[cellReceiving] > 0,
                                              fromNeighbor / missing[cellReceiving],
                                            0)
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

        ###################################################
        ### Iterations of Neighbor Cell Water Provision ###
        ###################################################
        fromWW <- fromWC <- toWW <- toWC <- numeric(l)
        # Loop through all neighboring cells within
        # certain distance sorted by distance
        for (i in 1:max(lengths(rs$neighborcell))) {

          # initialize requested water
          tmpRequestWWlocal <- numeric(l)
          tmpRequestWClocal <- numeric(l)

          # exclude cells with insufficient available water
          flag <- which(tmpDischarge - tmpPrevWW < 0)
          flaggedEmpty <- numeric(l)
          for (j in flag) {
            flaggedEmpty[c(j, rs$upstreamcells[[j]])] <- 1
          }
          # neighbor cells that will be skipped
          nskipped <- numeric(l)

          # Loop over main cells (k) to request water from neighbors (n)
          requestingList <- vector("list", l)
          for (k in 1:l) {

            if (tmpMissWW[k] > 0 &&
                !is.null(rs$neighborcell[[k]]) &&
                !is.na(rs$neighborcell[[k]][nskipped[k] + i])) {

              # Select cells that may provide water
              n <- rs$neighborcell[[k]][nskipped[k] + i]
              while (!is.na(n) && flaggedEmpty[n]) {
                  nskipped[k] <- nskipped[k] + 1
                  n <- rs$neighborcell[[k]][nskipped[k] + i]
              }

              # Assign water requested by main cell k from neighboring cell n
              if (!is.na(n)) {
                tmpRequestWWlocal[n] <- tmpRequestWWlocal[n] + tmpMissWW[k]
                tmpRequestWClocal[n] <- tmpRequestWClocal[n] + tmpMissWC[k]

                # Extract list of cells that have requested water from neighbor cell
                if (is.null(requestingList[[n]])) {
                  requestingList[[n]] <- k
                } else {
                  requestingList[[n]] <- c(requestingList[[n]], k)
                }
              }
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

          # Repeat Upstream-Downstream Reservation for
          # neighboring cells
          for (c in cellsCalc) {

            if ((tmpRequestWWlocal[c] > 0) ||
                ((tmpDischarge[c] + tmpPrevWC[c]) < tmpPrevWW[c])) {
              # Select cells for subsetting objects
              cellsRequest <- cellsDischarge <- c
              if (length(rs$upstreamcells[[c]]) > 0) {
                cellsRequest <- c(cellsRequest, unlist(rs$upstreamcells[[c]]))
              }
              if (length(rs$downstreamcells[[c]]) > 0) {
                cellsDischarge <- c(cellsDischarge, unlist(rs$downstreamcells[[c]]))
              }

              # Reserved Water Use Accounting
              tmp <- toolRiverUpDownBalanceSINGLE(inLIST = list(prevWC = tmpPrevWC[c],
                                                                prevWW = tmpPrevWW[c],
                                                                currWW = tmpRequestWWlocal[c]),
                                                  inoutLIST = list(q = tmpDischarge[cellsDischarge],
                                                                   currWC = tmpRequestWClocal[cellsRequest]))

              # Updated flows
              tmpDischarge[cellsDischarge]    <- tmp$q
              tmpRequestWClocal[cellsRequest] <- tmp$currWC
            }
          }
          # Water reserved in this round is reserved as previous use
          # before next round of neighbor water provision
          tmpPrevWC <- tmpPrevWC + tmpRequestWClocal
          fracFulfilled <- ifelse(tmpRequestWCtotal > 0,
                                     tmpRequestWClocal / tmpRequestWCtotal,
                                  0)
          tmpRequestWWlocal <- fracFulfilled * tmpRequestWWtotal
          tmpPrevWW <- tmpPrevWW + tmpRequestWWlocal

          # Update discharge given reserved water (consumptive)
          tmpDischarge <- toolRiverDischargeUpdate(rs = rs,
                                                   runoffWOEvap = listNeighborIN$runoffWOEvap[, y, scen],
                                                   watCons = tmpPrevWC)

          # Assign reserved flows to cell that had requested the water
          fracFromNeighbor <- .assignToMain(requestingList = requestingList,
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
