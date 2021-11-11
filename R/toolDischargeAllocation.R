#' @title       toolDischargeAllocation
#' @description This tool function executes the Allocation Algorithm in
#'              cell order of selected allocation rule
#'
#' @param y              Current year of loop
#' @param rs             River structure (list of river structure details and
#'                       cell numbers including ordered cell number)
#' @param inoutLIST      List of inputs that are also and output of the function
#'                       (variables that are updated in the discharge allocation loop):
#'                       reservedWAT (minimum water requirement reserved per cell);
#'                       discharge (discharge to be allocated);
#'                       fullirrigFRAC (fraction of full irrigation requirements that can be fulfilled)
#' @param inLIST         List of inputs to the discharge allocation algorithm
#'                       (variables that are not altered by the discharge allocation algorithm):
#'                       yieldgain (irrigation yield value gain);
#'                       fullIrrigReqWW (required withdrawal for full irrigation in specific cell);
#'                       fullIrrigReqWC (required consumption for full irrigation in specific cell);
#'                       gainthreshold (Minimum yield gain in USD/ha (as scalar value));
#'                       iwpWW; iwpWC (irrigation water potential (WW: withdrawal, WC: consumption),
#'                       i.e. water that is potentially available for irrigation water withdrawal/consumption)
#' @param allocationrule Rule to be applied for river basin discharge allocation
#'                       across cells of river basin ("optimization", "upstreamfirst")
#' @param glocellrank    Cell ranking for different years
#'                       Note: only applicable when allocationrule "optimization" chosen
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolDischargeAllocation <- function(y, rs, inoutLIST, inLIST, allocationrule, glocellrank) {

  # Cell ordering to be applied for surplus discharge allocation rules
  if (allocationrule == "optimization") {

    # Retrieve arguments
    fullIrrigReqWW <- inLIST$fullIrrigReqWW
    fullIrrigReqWC <- inLIST$fullIrrigReqWC
    yieldgain      <- inLIST$yieldgain
    iwpWW          <- inLIST$iwpWW
    iwpWC          <- inLIST$iwpWC
    scenarios      <- inLIST$scenarios

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    if (inLIST$multicropping) {

      # allocation share needs to be adjusted for multicropping length
      tmp <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
      tmp <- range(tmp)[2]

      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / tmp / 67420)

    } else {

      # allocation share depends on chosen cellranking
      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / 67420)

    }
    fullIrrigReqWW <- fullIrrigReqWW * allocationshare
    fullIrrigReqWC <- fullIrrigReqWC * allocationshare

    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    discharge     <- inoutLIST$discharge
    reservedWAT   <- inoutLIST$reservedWAT
    fullirrigFRAC <- inoutLIST$fullirrigFRAC

    for (o in (1:max(glocellrank[, y, ], na.rm = TRUE))) { # test <- rs$cells[order(glocellrank[,y,])]

      # Extract the cell number
      c <- rs$cells[rs$coordinates == paste(strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][1],
                                            strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][2],
                                            sep = ".")]

      # Extract season
      if (inLIST$multicropping) {

        if (grepl("S_", names(which(glocellrank[, y, ] == o)))) {
          season <- paste(scenarios, "single", sep = ".")
        } else if (grepl("D_", names(which(glocellrank[, y, ] == o)))) {
          season <- paste(scenarios, "double", sep = ".")
        } else if (grepl("T_", names(which(glocellrank[, y, ] == o)))) {
          season <- paste(scenarios, "triple", sep = ".")
        } else {
          stop("Object glocellrank does not have multicropping dimensions")
        }

      } else {
        season <- paste(scenarios, "single", sep = ".")
      }

      # Helper vectors for subsetting of objects
      # vector of downstreamcells of c
      vDOWN <- unlist(rs$downstreamcells[[c]])
      # vector of c in length of downstreamcells of c
      vCELL <- rep(c, length(rs$downstreamcells[[c]]))
      # vector of 1s in length of downstreamcells of c
      vONES <- rep(1:length(c), length(rs$downstreamcells[[c]]))

      # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
      is_gain <- (yieldgain[c, y, season, drop = F] > inLIST$gainthreshold)

      # irrigation water potential (available water for additional irrigation withdrawals)
      iwpWW[c, y, ][is_gain[, , , drop = F]] <- pmax(discharge[c, y, , drop = F] -
                                                      reservedWAT[c, y, , drop = F],
                                                     0)[is_gain[, , , drop = F]]

      # withdrawal constraint (if there is water required for withdrawal in current grid cell)
      is_req_ww   <- (fullIrrigReqWW[c, y, season, drop = F] > 0 & is_gain[, , , drop = F])

      # how much withdrawals can be fulfilled by available water
      fullirrigFRAC[c, y, season][is_req_ww[, , , drop = F]] <- pmin(iwpWW[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                                         fullIrrigReqWW[c, y, season, drop = F][is_req_ww[, , , drop = F]],
                                                                    1)

      if (length(vDOWN) > 0) {
        # consumption constraint (if there is water required for consumption in current grid cell)
        is_req_wc <- (fullIrrigReqWC[c, y, season, drop = F] > 0 & is_req_ww[, , , drop = F])

        # irrigation water potential (available water for additional irrigation consumption (considering downstream availability))
        iwpWC[c, y, ][is_req_wc[, , , drop = F]] <- pmax(apply(discharge[vDOWN, y, , drop = F] -
                                                                    reservedWAT[vDOWN, y, , drop = F], MARGIN = 3, min),
                                                              0)[is_req_wc[, , , drop = F]]

        # how much consumption can be fulfilled by available water
        fullirrigFRAC[c, y, season][is_req_wc[, , , drop = F]] <- pmin(iwpWC[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                       fullIrrigReqWC[c, y, season, drop = F][is_req_wc[, , , drop = F]],
                                                                     fullirrigFRAC[c, y, season, drop = F][is_req_wc[, , , drop = F]])
      }

      # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
      discharge[c(vDOWN, c), y, ][is_req_ww[c(vONES, 1), , , drop = F]] <- (discharge[c(vDOWN, c), y, , drop = F] -
                                                                                 fullIrrigReqWC[c(vCELL, c), y, season, drop = F] * fullirrigFRAC[c(vCELL, c), y, season, drop = F])[is_req_ww[c(vONES, 1), , , drop = F]]
      # update minimum water required in cell:
      reservedWAT[c, y, ][is_req_ww[, , , drop = F]]      <- (reservedWAT[c, y, , drop = F] +
                                                                                 fullirrigFRAC[c, y, season, drop = F] * fullIrrigReqWW[c, y, season, drop = F])[is_req_ww[, , , drop = F]]
    }

    inoutLIST <- list(discharge = discharge,
                    reservedWAT = reservedWAT,
                    fullirrigFRAC = fullirrigFRAC)

  } else if (allocationrule == "upstreamfirst") {

    fullIrrigReqWW <- inLIST$fullIrrigReqWW
    fullIrrigReqWC <- inLIST$fullIrrigReqWC
    yieldgain      <- inLIST$yieldgain
    iwpWW          <- inLIST$iwpWW
    iwpWC          <- inLIST$iwpWC
    scenarios      <- inLIST$scenarios

    discharge      <- inoutLIST$discharge
    reservedWAT    <- inoutLIST$reservedWAT
    fullirrigFRAC  <- inoutLIST$fullirrigFRAC


    # Extract season
    if (inLIST$multicropping) {
      s <- c("single", "double", "triple")
    } else {
      s <- "single"
    }

    # Allocate first, then second, then third season
    for (season in s) {

      season <- paste(scenarios, season, sep = ".")

      for (o in 1:max(rs$calcorder)) {
        cells <- which(rs$calcorder == o)

        for (c in cells) {

          # Helper vectors for subsetting of objects
          # vector of downstreamcells of c
          vDOWN <- unlist(rs$downstreamcells[[c]])
          # vector of c in length of downstreamcells of c
          vCELL <- rep(c, length(rs$downstreamcells[[c]]))
          # vector of 1s in length of downstreamcells of c
          vONES <- rep(1:length(c), length(rs$downstreamcells[[c]]))

          # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
          is_gain <- (yieldgain[c, y, season, drop = F] > inLIST$gainthreshold)

          # available water for additional irrigation withdrawals
          iwpWW[c, y, ][is_gain[, , , drop = F]] <- pmax(discharge[c, y, , drop = F] -
                                                                reservedWAT[c, y, , drop = F],
                                                              0)[is_gain[, , , drop = F]]

          # withdrawal constraint (if there is water required for withdrawal in current grid cell)
          is_req_ww   <- (fullIrrigReqWW[c, y, season, drop = F] > 0 & is_gain[, , , drop = F])

          # how much withdrawals can be fulfilled by available water
          fullirrigFRAC[c, y, season][is_req_ww[, , , drop = F]] <- pmin(iwpWW[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                                         fullIrrigReqWW[c, y, season, drop = F][is_req_ww[, , , drop = F]],
                                                                       1)

          if (length(vDOWN) > 0) {
            # consumption constraint (if there is water required for consumption in current grid cell)
            is_req_wc <- (fullIrrigReqWC[c, y, season, drop = F] > 0 & is_req_ww[, , , drop = F])

            # available water for additional irrigation consumption (considering downstream availability)
            iwpWC[c, y, ][is_req_wc[, , , drop = F]]     <- pmax(apply(discharge[vDOWN, y, , drop = F] -
                                                                        reservedWAT[vDOWN, y, , drop = F], MARGIN = 3, min),
                                                                      0)[is_req_wc[, , , drop = F]]

            # how much consumption can be fulfilled by available water
            fullirrigFRAC[c, y, season][is_req_wc[, , , drop = F]] <- pmin(iwpWC[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                           fullIrrigReqWC[c, y, season, drop = F][is_req_wc[, , , drop = F]],
                                                                         fullirrigFRAC[c, y, season, drop = F][is_req_wc[, , , drop = F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          discharge[c(vDOWN, c), y, ][is_req_ww[c(vONES, 1), , , drop = F]] <- (discharge[c(vDOWN, c), y, , drop = F] -
                                                                                fullIrrigReqWC[c(vCELL, c), y, season, drop = F] * fullirrigFRAC[c(vCELL, c), y, season, drop = F])[is_req_ww[c(vONES, 1), , , drop = F]]
          # update minimum water required in cell:
          reservedWAT[c, y, ][is_req_ww[, , , drop = F]]    <- (reservedWAT[c, y, , drop = F] +
                                                                fullirrigFRAC[c, y, season, drop = F] * fullIrrigReqWW[c, y, season, drop = F])[is_req_ww[, , , drop = F]]
        }

        inoutLIST <- list(discharge = discharge,
                          reservedWAT = reservedWAT,
                          fullirrigFRAC = fullirrigFRAC)
      }
    }
  }

  return(inoutLIST)
}
