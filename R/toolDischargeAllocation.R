#' @title       toolDischargeAllocation
#' @description This tool function executes the Allocation Algorithm in
#'              cell order of selected allocation rule
#'
#' @param y              Current year of loop
#' @param rs             River structure (list of river structure details and
#'                       cell numbers including ordered cell number)
#' @param inoutLIST      List of inputs that are at the same time outputs:
#'                       minWatReserved Minimum (water requirement reserved
#'                                      per grid cell (as magpie object of correct dimension));
#'                       discharge (Discharge to be allocated (as magpie object of correct dimension));
#'                       fracFullirrig (fraction of fullirrigation requirements that can be fulfilled)
#' @param inLIST         List of inputs:
#'                       irrigGain (yield gain potential through irrigation of proxy crops:
#'                                 magpie object with cellular and year dimension
#'                                 (as magpie object of correct dimension));
#'                       reqWatFullirrigWW (required withdrawal for full irrigation
#'                                          in specific cell (as magpie object of correct dimension));
#'                       reqWatFullirrigWC (required consumption for full irrigation
#'                                          in specific cell (as magpie object of correct dimension));
#'                       gainthreshold (Minimum yield gain in USD/ha (as scalar value)); avlWatWW; avlWatWC
#' @param allocationrule Rule to be applied for river basin discharge allocation
#'                       across cells of river basin ("optimization", "upstreamfirst")
#' @param glocellrank    Cell ranking for different years (array).
#'                       Note: only applicable when allocationrule "optimization" chosen
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolDischargeAllocation <- function(y, rs, inoutLIST, inLIST,
                                    allocationrule, glocellrank) {

  # Cell ordering to be applied for surplus discharge allocation rules
  if (allocationrule == "optimization") {

    # Retrieve arguments
    reqWatFullirrigWW <- inLIST$reqWatFullirrigWW
    reqWatFullirrigWC <- inLIST$reqWatFullirrigWC
    irrigGain         <- inLIST$irrigGain
    avlWatWW          <- inLIST$avlWatWW
    avlWatWC          <- inLIST$avlWatWC

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    allocationshare   <- 1 / (length(glocellrank[, 1, 1]) / 67420)
    reqWatFullirrigWW <- reqWatFullirrigWW * allocationshare
    reqWatFullirrigWC <- reqWatFullirrigWC * allocationshare

    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    dischargeNEW <- inoutLIST$discharge
    reservedNEW  <- inoutLIST$minWatReserved
    fracNEW      <- inoutLIST$fracFullirrig

    for (o in (1:max(glocellrank[, y, ], na.rm = TRUE))) {

      # Extract the cell number
      c <- rs$cells[rs$coordinates == paste(strsplit(gsub(".*_", "",
                                                          names(which(glocellrank[, y, ] == o))), "\\.")[[1]][1],
                                            strsplit(gsub(".*_", "",
                                                          names(which(glocellrank[, y, ] == o))), "\\.")[[1]][2],
                                            sep = ".")]

      ### Potential Function Improvements:
      # (1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS

      # Helper vectors for subsetting of objects
      # vector of downstreamcells of c
      vDOWN <- unlist(rs$downstreamcells[[c]])
      # vector of c in length of downstreamcells of c
      vCELL <- rep(c, length(rs$downstreamcells[[c]]))
      # vector of 1s in length of downstreamcells of c
      vONES <- rep(1, length(rs$downstreamcells[[c]]))

      # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
      isGAIN <- (irrigGain[c, y, , drop = FALSE] > inLIST$gainthreshold)

      # available water for additional irrigation withdrawals
      avlWatWW[c, y, ][isGAIN] <- pmax(dischargeNEW[c, y, , drop = FALSE] -
                                         reservedNEW[c, y, , drop = FALSE],
                                       0)[isGAIN]

      # withdrawal constraint (if there is water required for withdrawal in current grid cell)
      isWW   <- (reqWatFullirrigWW[c, y, , drop = FALSE] > 0 & isGAIN)

      # how much withdrawals can be fulfilled by available water
      fracNEW[c, y, ][isWW] <- pmin(avlWatWW[c, y, , drop = FALSE][isWW] /
                                       reqWatFullirrigWW[c, y, , drop = FALSE][isWW],
                                    1)

      if (length(vDOWN) > 0) {
        # consumption constraint (if there is water required for consumption in current grid cell)
        isWC <- (reqWatFullirrigWC[c, y, , drop = FALSE] > 0 & isWW)

        # available water for additional irrigation consumption (considering downstream availability)
        avlWatWC[c, y, ][isWC] <- pmax(apply(dischargeNEW[vDOWN, y, , drop = FALSE] -
                                                reservedNEW[vDOWN, y, , drop = FALSE],
                                             MARGIN = 3, min),
                                       0)[isWC]

        # how much consumption can be fulfilled by available water
        fracNEW[c, y, ][isWC] <- pmin(avlWatWC[c, y, , drop = FALSE][isWC] /
                                         reqWatFullirrigWC[c, y, , drop = FALSE][isWC],
                                      fracNEW[c, y, , drop = FALSE][isWC])
      }

      # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
      dischargeNEW[c(vDOWN, c), y, ][isWW[c(vONES, 1), , ,
                                          drop = FALSE]] <- (dischargeNEW[c(vDOWN, c), y, , drop = FALSE] -
                                                              reqWatFullirrigWC[c(vCELL, c), y, , drop = FALSE] *
                                                                fracNEW[c(vCELL, c), y, ,
                                                                        drop = FALSE])[isWW[c(vONES, 1), , ,
                                                                                            drop = FALSE]]
      # update minimum water required in cell:
      reservedNEW[c, y, ][isWW] <- (reservedNEW[c, y, , drop = FALSE] +
                                     fracNEW[c, y, , drop = FALSE] * reqWatFullirrigWW[c, y, , drop = FALSE])[isWW]
    }

    inoutLIST <- list(discharge = dischargeNEW,
                      minWatReserved = reservedNEW,
                      fracFullirrig = fracNEW)

  } else if (allocationrule == "upstreamfirst") {

    reqWatFullirrigWW <- inLIST$reqWatFullirrigWW
    reqWatFullirrigWC <- inLIST$reqWatFullirrigWC
    irrigGain         <- inLIST$irrigGain
    avlWatWW          <- inLIST$avlWatWW
    avlWatWC          <- inLIST$avlWatWC

    dischargeNEW      <- inoutLIST$discharge
    reservedNEW       <- inoutLIST$minWatReserved
    fracNEW           <- inoutLIST$fracFullirrig

    for (o in 1:max(rs$calcorder)) {
      cells <- which(rs$calcorder == o)

      for (c in cells) {
        ### Potential Function Improvements:
        # (1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS

        # Helper vectors for subsetting of objects
        # vector of downstreamcells of c
        vDOWN <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        vCELL <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1s in length of downstreamcells of c
        vONES <- rep(1, length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        isGAIN <- (irrigGain[c, y, , drop = FALSE] > inLIST$gainthreshold)

        # available water for additional irrigation withdrawals
        avlWatWW[c, y, ][isGAIN] <- pmax(dischargeNEW[c, y, , drop = FALSE] -
                                           reservedNEW[c, y, , drop = FALSE],
                                         0)[isGAIN]

        # withdrawal constraint (if there is water required for withdrawal in current grid cell)
        isWW   <- (reqWatFullirrigWW[c, y, , drop = FALSE] > 0 & isGAIN)

        # how much withdrawals can be fulfilled by available water
        fracNEW[c, y, ][isWW] <- pmin(avlWatWW[c, y, , drop = FALSE][isWW] /
                                        reqWatFullirrigWW[c, y, , drop = FALSE][isWW],
                                      1)

        if (length(vDOWN) > 0) {
          # consumption constraint (if there is water required for consumption in current grid cell)
          isWC <- (reqWatFullirrigWC[c, y, , drop = FALSE] > 0 & isWW)

          # available water for additional irrigation consumption (considering downstream availability)
          avlWatWC[c, y, ][isWC] <- pmax(apply(dischargeNEW[vDOWN, y, , drop = FALSE] -
                                                  reservedNEW[vDOWN, y, , drop = FALSE],
                                               MARGIN = 3, min),
                                         0)[isWC]

          # how much consumption can be fulfilled by available water
          fracNEW[c, y, ][isWC]  <- pmin(avlWatWC[c, y, , drop = FALSE][isWC] /
                                           reqWatFullirrigWC[c, y, , drop = FALSE][isWC],
                                         fracNEW[c, y, , drop = FALSE][isWC])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        dischargeNEW[c(vDOWN, c), y, ][isWW[c(vONES, 1), , ,
                                            drop = FALSE]] <- (dischargeNEW[c(vDOWN, c), y, , drop = FALSE] -
                                                                reqWatFullirrigWC[c(vCELL, c), y, , drop = FALSE] *
                                                                 fracNEW[c(vCELL, c), y, ,
                                                                         drop = FALSE])[isWW[c(vONES, 1), , ,
                                                                                             drop = FALSE]]
        # update minimum water required in cell:
        reservedNEW[c, y, ][isWW] <- (reservedNEW[c, y, , drop = FALSE] +
                                       fracNEW[c, y, , drop = FALSE] * reqWatFullirrigWW[c, y, , drop = FALSE])[isWW]
      }

      inoutLIST <- list(discharge = dischargeNEW,
                        minWatReserved = reservedNEW,
                        fracFullirrig = fracNEW)
    }
  }

  return(inoutLIST)
}
