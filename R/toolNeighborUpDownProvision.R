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
#' calcOutput("RiverHumanUses", aggregate = FALSE)
#' }
#'
#' @export

toolNeighborUpDownProvision <- function(rs, transDist,
                                        years, scenarios,
                                        listNeighborIN) {

  # set tolerance
  epsilon <- 1e-6

  # read-in inputs
  prevWC <- listNeighborIN$prevReservedWC
  prevWW <- listNeighborIN$prevReservedWW
  missWW <- listNeighborIN$missingWW
  missWC <- listNeighborIN$missingWC
  dischargeOLD <- listNeighborIN$discharge

  l <- length(rs$cells)
  names(rs$neighborcell) <- 1:l
  names(rs$neighbordist) <- 1:l

  ### Internal Function ###
  .assignToMain <- function(requestingList, epsilon,
                            missing, toNeighbor) {
    fracAssigned <- numeric(length(missing))
    cellsGiving <- which(toNeighbor > 0)

    for (s in cellsGiving) {
      cellReceiving <- requestingList[[s]]
      shr <- missing[cellReceiving] / sum(missing[cellReceiving])

      fromNeighbor <- shr * toNeighbor[s]

      fracAssigned[cellReceiving] <- ifelse(missing[cellReceiving] > epsilon,
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
        tmpDischarge <- iniDischarge <- dischargeOLD[, y, scen]
        tmpRequestWWlocal <- numeric(l)
        tmpRequestWClocal <- numeric(l)
        tmpMissWW <- missWW[, y, scen]
        tmpMissWC <- missWC[, y, scen]

        # exclude cells with insufficient available water
        flag <- which(dischargeOLD[, y, scen] - prevWW[, y, scen] < epsilon)

        flaggedEmpty <- numeric(l)
        for (j in flag) {
            flaggedEmpty[c(j, rs$upstreamcells[[j]])] <- 1
        }

        nskipped <- numeric(l)

        # Loop through all neighboring cells within
        # certain distance sorted by distance
        for (i in 1:max(lengths(rs$neighborcell))) {

            print(paste0(i, "th round of neighbor water provision (loop over i)"))

            requestingList <- vector("list", l)
            for (k in 1:l) {

                if (tmpMissWW[k] > epsilon &&
                !is.null(rs$neighborcell[[k]]) &&
                !is.na(rs$neighborcell[[k]][nskipped[k] + i])) {

                    n <- rs$neighborcell[[k]][nskipped[k] + i]

                    while (!is.na(n) && flaggedEmpty[n]) {
                        nskipped[k] <- nskipped[k] + 1
                        n <- rs$neighborcell[[k]][nskipped[k] + i]
                    }

                    if (!is.na(n)) {
                        tmpRequestWWlocal[n] <- tmpRequestWWlocal[n] + tmpMissWW[k]
                        tmpRequestWClocal[n] <- tmpRequestWClocal[n] + tmpMissWC[k]

                        if (is.null(requestingList[[n]])) {
                          requestingList[[n]] <- k
                        } else {
                          requestingList[[n]] <- c(requestingList[[n]], k)
                        }
                    }
                }
            }
            currRequestWCtotal <- tmpRequestWClocal
            currRequestWWtotal <- tmpRequestWWlocal

            cellsCalc <- which(tmpRequestWWlocal > epsilon)
            cellsCalc <- unique(c(cellsCalc, unlist(rs$downstreamcells[cellsCalc])))
            cellsCalc <- cellsCalc[order(rs$calcorder[cellsCalc], decreasing = FALSE)]
            # For performance test
            # cellsCalc <- 1:l
            # cellsCalc <- cellsCalc[order(rs$calcorder[cellsCalc], decreasing = FALSE)]
            # Test this for where we have the double loop

            print(paste0("The number of calculated cells are: ", length(cellsCalc)))
            print(paste0("Remaining missing water is: ", round(sum(tmpMissWC))))
            print(paste0("Sum requested from neighbor is: ", round(sum(tmpRequestWClocal))))
            print(paste0("Number of real neighbors: ", sum(tmpRequestWClocal > epsilon)))

            # Repeat Upstream-Downstream Reservation for
            # neighboring cells
            for (c in cellsCalc) {

                if ((tmpRequestWWlocal[c] > epsilon) ||
                    (tmpDischarge[c] <= prevWW[c, y, scen])) {

                    cellsRequest <- cellsDischarge <- c
                    if (length(rs$upstreamcells[[c]]) > 0) {
                      cellsRequest <- c(cellsRequest, unlist(rs$upstreamcells[[c]]))
                    }
                    if (length(rs$downstreamcells[[c]]) > 0) {
                      cellsDischarge <- c(cellsDischarge, unlist(rs$downstreamcells[[c]]))
                    }
                    cellsDischarge <- unique(c(cellsRequest, cellsDischarge))

                    tmp <- toolRiverUpDownBalanceSINGLE(inLIST = list(currWW = tmpRequestWWlocal[c],
                                                                      prevWW = prevWW[c, y, scen]),
                                                        inoutLIST = list(q = tmpDischarge[cellsDischarge],
                                                                         currWC = tmpRequestWClocal[cellsRequest]))

                    # Updated flows
                    tmpDischarge[cellsDischarge] <- tmp$q
                    tmpRequestWClocal[cellsRequest] <- tmp$currWC
                }
            }

            fracFromNeighbor <- .assignToMain(requestingList = requestingList,
                                                        missing = tmpMissWC,
                                                        toNeighbor = tmpRequestWClocal)

           # stop("Stopp")

            # Update water that is still missing in main river cell(s)
            tmpMissWW <- tmpMissWW * (1 - fracFromNeighbor)
            tmpMissWC <- tmpMissWC * (1 - fracFromNeighbor)

            # repeat until no more missing water OR no more neighbor cells
            if (all(tmpMissWW <= 0) && all(tmpMissWC <= 0)) {
              break
            }
        }

        # Update discharge
        discharge[, y, scen] <- tmpDischarge
        currRequestWClocal[, y, scen] <- tmpRequestWClocal
        missWW[, y, scen] <- tmpMissWW
        missWC[, y, scen] <- tmpMissWC
     }
   }

               # Update local water consumption/withdrawal
               fracFulfilled <- tmpRequestWClocal / currRequestWCtotal
               fracFulfilled[currRequestWCtotal == 0] <- 0

               currRequestWClocal <- fracFulfilled * currRequestWCtotal
               currRequestWWlocal <- fracFulfilled * currRequestWWtotal

               toNeighborWW <- toNeighborWC <- fromNeighborWW <- fromNeighborWC <- 0 # assign somewhere on top

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
