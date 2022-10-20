#' @title       toolNeighborUpDownProvision
#' @description This function calculates water provision by 
#'              surrounding grid cells for upstream-downstream
#'              allocation set-up
#'
#' @param rs             River structure including information 
#'                       on neighboring cells
#' @param transDist      Water transport distance allowed to fulfill locally
#'                       unfulfilled water demand by surrounding cell water availability
#' @param listNeighborIN List of arrays required for the algorithm:
#'                       yearlyRunoff, lakeEvap
#'                       reserved flows from previous water allocation round 
#'                       (prevReservedWC, prevReservedWW)
#'                       missing water at this stage of water allocation 
#'                       (missingWW, missingWC)
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
                                        listNeighborIN) {

  # read-in inputs
  runoff <- listNeighborIN$yearlyRunoff
  evap   <- listNeighborIN$lakeEvap
  prevWC <- listNeighborIN$prevReservedWC
  prevWW <- listNeighborIN$prevReservedWW
  missWW <- listNeighborIN$missingWW
  missWC <- listNeighborIN$missingWC

  l <- length(rs$cells)
  names(rs$neighborcell) <- 1:l
  names(rs$neighbordist) <- 1:l

  # initialize objects to be filled in correct dimension
  tmp       <- runoff
  tmp[, , ] <- 0
  inflow <- discharge <- currRequestWWlocal <- currRequestWClocal <- tmp
  fromNeighborWW <- fromNeighborWC <- tmp

  # Internal function assigning reserved water
  # in neighboring cell to respective main river cell
  .assignTOmain <- function(i, missing, toNeighbor, fromNeighbor) {

    ### This function is a bottleneck. Takes very long...
    ### @JAN/JENS: How to improve? Maybe avoid for-loops, but not sure how?

    # loop through all cells that have missing water
    for (s in 1:l) {
    
      # main cells that requested water from s
      j <- as.numeric(names(which(unlist(lapply(rs$neighborcell, "[", i)) == s)))

      if (!identical(j, numeric(0)) && any(missing[j, , ] != 0)) {

        # order by distance
        dist <- as.numeric(sort(unlist(lapply(rs$neighbordist[j], "[", i))))
        j    <- as.numeric(names(sort(unlist(lapply(rs$neighbordist[j], "[", i)))))

        checkStep <- 0
        for (m in 1:length(j)) {

          # Ensure that cell has not yet been calculated 
          # because of equal distance
          if (checkStep != 0) {
            checkStep <- checkStep - 1
            next
          }

          # If more than one cell has equal distance:
          # allocate proportionally
          if (length(j[dist == dist[m]]) != 1) {
            # vector of s in correct length
            vS <- rep(s, length(j[dist == dist[m]]))
            # temporary variable
            shr       <- toNeighbor
            shr[, , ] <- 0
            # share to be allocated to each cell
            allWW <- colSums(missing[j[dist == dist[m]], , , drop = FALSE],
                            dims = 1)
            shr[s, , ][allWW != 0] <- (toNeighbor[s, , ] / allWW)[allWW != 0]

            # allocation to respective neighboring cells
            allocated <- missing[j[dist == dist[m]], , , drop = FALSE] * shr[vS, , , drop = FALSE]
            fromNeighbor[j[dist == dist[m]], , ] <- fromNeighbor[j[dist == dist[m]], , , drop = FALSE] +
                                                      allocated
            toNeighbor[s, , ] <- toNeighbor[s, , ] -
                                    colSums(allocated, dims = 1)

            # jump to next (all that had same distance should not be allocated again)
            checkStep <- length(j[dist == dist[m]]) - 1
            ### @JAN / @JENS: Does that make sense?
            rm(allocated)

          } else {
            # If distance is unique:
            # allocate to closest first
            allocated <- pmin(missing[j[m], , , drop = FALSE],
                                toNeighbor[s, , , drop = FALSE])
            fromNeighbor[j[m], , ] <- fromNeighbor[j[m], , , drop = FALSE] +
                                        allocated

            toNeighbor[s, , ] <- toNeighbor[s, , , drop = FALSE] -
                                    allocated
            rm(allocated)
          }
        }

        if (any(toNeighbor[s, , ] <= 0)) {
          if (any(toNeighbor[s, , ] < 0)) {
            stop("Too much water was taken from Neighbor in toolNeighborUpDownProvision")
          }
          break
        }
      }
    }
    return(list(toNeighbor = toNeighbor,
                fromNeighbor = fromNeighbor))
  }

  # Loop through all neighboring cells within
  # certain distance sorted by distance
  for (i in 1:max(lengths(rs$neighborcell))) {

    #@JENS/@JAN: This is a bottleneck...
    # max is 226 --> calculations would repeat 226 times, even if it's only
    # one or two cells that have that many neighbor cells... How to improve?

    print(paste0(i, "th round of neighbor water provision (loop over i)"))

    # Assign missing water to clostest neighboring cell
    # @JENS: What about two with same distance?
    currRequestWWlocal[, , ] <- 0
    currRequestWClocal[, , ] <- 0

    for (k in 1:l) {
      n <- rs$neighborcell[[k]][i]

      if (!is.null(n) && !is.na(n)) {
        currRequestWWlocal[n, , ] <- currRequestWWlocal[n, , ] + missWW[k, , ]
        currRequestWClocal[n, , ] <- currRequestWClocal[n, , ] + missWC[k, , ]
        ### @JENS: a little bit of missWW is not allocated to currRequestWWlocal
        ###       (probably because there are no neighbors?)
        # e.g. for case non_agriculture:
        # > sum(missWW[,,"off.ssp2"])
        # [1] 86235.61
        #> sum(currRequestWWlocal[,,"off.ssp2"])
        # [1] 86206.43
      }
    }
    
    #### HERE MIGHT BE IMPROVEMENT POTENTIAL!
    # instead of running over all cells again, determine where there has been
    # a request
    # calculate toolRiverUpDownBalance for that cell and all downstream cells of it.
    # (performance gains further down the line)

    # Repeat Upstream-Downstream Reservation for
    # neighboring cells
    for (o in 1:max(rs$calcorder)) {
      # Note: the calcorder ensures that the upstreamcells are calculated first
      cells <- which(rs$calcorder == o)

      for (c in cells) {

        if (any(currRequestWWlocal[c, , ] != 0) || any(currRequestWClocal[c, , ] != 0)) {

          for (k in c(c, rs$downstreamcells[c])) {
            #@JENS: Does that make sense? I would only calculate again if there is actually missing water (curReq!=0)
            # and then I would only calculate for that specific cell and its downstream cells. (because inflow is handed over)
            # Or would I have to again calculate for all/more (e.g. also upstream cells)?
            tmp <- toolRiverUpDownBalance(c = c, rs = rs,
                                          inLIST = list(yearlyRunoff = runoff[c, , , drop = FALSE],
                                                        lakeEvap = evap[c, , , drop = FALSE],
                                                        prevReservedWW = prevWW,
                                                        prevReservedWC = prevWC[c, , , drop = FALSE]),
                                          inoutLIST = list(discharge = discharge,
                                                          inflow = inflow,
                                                          currRequestWClocal = currRequestWClocal,
                                                          currRequestWWlocal = currRequestWWlocal))
            # Updated flows
            discharge    <- tmp$discharge
            inflow       <- tmp$inflow
            currRequestWWlocal <- tmp$currRequestWWlocal
            currRequestWClocal <- tmp$currRequestWClocal
        }
       }
      }
    }
    toNeighborWW <- currRequestWWlocal
    toNeighborWC <- currRequestWClocal

    ### Assign reserved water to respective main river cell(s) ###
    ### that had requested the water                           ###
    print(paste0("Assigning reserved water in ", i, "th round of neighbor water provision. Step A: WW"))
    tmp <- .assignTOmain(i = i, missing = missWW,
                         toNeighbor = toNeighborWW,
                         fromNeighbor = fromNeighborWW)
    toNeighborWW   <- tmp$toNeighbor
    fromNeighborWW <- tmp$fromNeighbor
    rm(tmp)
    print(paste0("Assigning reserved water in ", i, "th round of neighbor water provision. Step B: WC"))
    tmp <- .assignTOmain(i = i, missing = missWC,
                        toNeighbor = toNeighborWC,
                        fromNeighbor = fromNeighborWC)
    toNeighborWC   <- tmp$toNeighbor
    fromNeighborWC <- tmp$fromNeighbor
    rm(tmp)

    # Update water that is still missing in main river cell(s)
    missWW <- missWW - fromNeighborWW
    missWC <- missWC - fromNeighborWC

    # repeat until no more missing water OR no more neighbor cells
    if (all(missWW <= 0) && all(missWC <= 0)) {
      break
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