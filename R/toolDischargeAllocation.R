#' @title       toolDischargeAllocation
#' @description This tool function executes the Allocation Algorithm in
#'              cell order of selected allocation rule
#'
#' @param y              Current year of loop
#' @param rs             River structure (list of river structure details and
#'                       cell numbers including ordered cell number)
#' @param l_inout        List of inputs that are at the same time outputs:
#'                       minWatReserved Minimum (water requirement reserved per grid cell (as magpie object of correct dimension));
#'                       discharge (Discharge to be allocated (as magpie object of correct dimension));
#'                       fracFullirrig (fraction of fullirrigation requirements that can be fulfilled)
#' @param l_in           List of inputs:
#'                       irrigGain (yield gain potential through irrigation of proxy crops: magpie object with cellular and year dimension (as magpie object of correct dimension));
#'                       reqWatFullirrigWW (required withdrawal for full irrigation in specific cell (as magpie object of correct dimension));
#'                       reqWatFullirrigWC (required consumption for full irrigation in specific cell (as magpie object of correct dimension));
#'                       gainthreshold (Minimum yield gain in USD/ha (as scalar value)); avlWatWW; avlWatWC
#' @param allocationrule Rule to be applied for river basin discharge allocation
#'                       across cells of river basin ("optimization", "upstreamfirst")
#' @param glocellrank    Cell ranking for different years (array).
#'                       Note: only applicable when allocationrule "optimization" chosen
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolDischargeAllocation <- function(y, rs, l_inout, l_in, allocationrule, glocellrank) {

  # Cell ordering to be applied for surplus discharge allocation rules
  if (allocationrule == "optimization") {

    # Retrieve arguments
    I_reqWatFullirrigWW <- l_in$reqWatFullirrigWW
    I_reqWatFullirrigWC <- l_in$reqWatFullirrigWC
    I_irrigGain         <- l_in$irrigGain
    avlWatWW            <- l_in$avlWatWW
    avlWatWC            <- l_in$avlWatWC

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    allocationshare     <- 1 / (length(glocellrank[, 1, 1]) / 67420)
    I_reqWatFullirrigWW <- I_reqWatFullirrigWW * allocationshare
    I_reqWatFullirrigWC <- I_reqWatFullirrigWC * allocationshare

    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    IO_discharge      <- l_inout$discharge
    IO_minWatReserved <- l_inout$minWatReserved
    IO_fracFullirrig  <- l_inout$fracFullirrig

    for (o in (1:max(glocellrank[, y, ], na.rm = TRUE))) { # test <- rs$cells[order(glocellrank[,y,])]

      # Extract the cell number
      c <- rs$cells[rs$coordinates == paste(strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][1],
                                            strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][2],
                                            sep = ".")]

      ### Potential Function Improvements:
      # (1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS

      # Helper vectors for subsetting of objects
      # vector of downstreamcells of c
      v_down <- unlist(rs$downstreamcells[[c]])
      # vector of c in length of downstreamcells of c
      v_cell <- rep(c, length(rs$downstreamcells[[c]]))
      # vector of 1s in length of downstreamcells of c
      v_ones <- rep(1:length(c), length(rs$downstreamcells[[c]]))

      # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
      is_gain <- (I_irrigGain[c, y, , drop = FALSE] > l_in$gainthreshold)

      # available water for additional irrigation withdrawals
      avlWatWW[c, y, ][is_gain[, , , drop = FALSE]] <- pmax(IO_discharge[c, y, , drop = FALSE] -
                                                          IO_minWatReserved[c, y, , drop = F],
                                                     0)[is_gain[, , , drop = F]]

      # withdrawal constraint (if there is water required for withdrawal in current grid cell)
      is_req_ww   <- (I_reqWatFullirrigWW[c, y, , drop = F] > 0 & is_gain[, , , drop = F])

      # how much withdrawals can be fulfilled by available water
      IO_fracFullirrig[c, y, ][is_req_ww[, , , drop = F]] <- pmin(avlWatWW[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                                         I_reqWatFullirrigWW[c, y, , drop = F][is_req_ww[, , , drop = F]],
                                                                    1)

      if (length(v_down) > 0) {
        # consumption constraint (if there is water required for consumption in current grid cell)
        is_req_wc <- (I_reqWatFullirrigWC[c, y, , drop = F] > 0 & is_req_ww[, , , drop = F])

        # available water for additional irrigation consumption (considering downstream availability)
        avlWatWC[c, y, ][is_req_wc[, , , drop = F]] <- pmax(apply(IO_discharge[v_down, y, , drop = F] -
                                                                    IO_minWatReserved[v_down, y, , drop = F], MARGIN = 3, min),
                                                              0)[is_req_wc[, , , drop = F]]

        # how much consumption can be fulfilled by available water
        IO_fracFullirrig[c, y, ][is_req_wc[, , , drop = F]] <- pmin(avlWatWC[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                       I_reqWatFullirrigWC[c, y, , drop = F][is_req_wc[, , , drop = F]],
                                                                     IO_fracFullirrig[c, y, , drop = F][is_req_wc[, , , drop = F]])
      }

      # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
      IO_discharge[c(v_down, c), y, ][is_req_ww[c(v_ones, 1), , , drop = F]] <- (IO_discharge[c(v_down, c), y, , drop = F] -
                                                                                 I_reqWatFullirrigWC[c(v_cell, c), y, , drop = F] * IO_fracFullirrig[c(v_cell, c), y, , drop = F])[is_req_ww[c(v_ones, 1), , , drop = F]]
      # update minimum water required in cell:
      IO_minWatReserved[c, y, ][is_req_ww[, , , drop = F]]      <- (IO_minWatReserved[c, y, , drop = F] +
                                                                                 IO_fracFullirrig[c, y, , drop = F] * I_reqWatFullirrigWW[c, y, , drop = F])[is_req_ww[, , , drop = F]]
    }

    l_inout <- list(discharge = IO_discharge,
                    minWatReserved = IO_minWatReserved,
                    fracFullirrig = IO_fracFullirrig)

  } else if (allocationrule == "upstreamfirst") {

    I_reqWatFullirrigWW <- l_in$reqWatFullirrigWW
    I_reqWatFullirrigWC <- l_in$reqWatFullirrigWC
    I_irrigGain         <- l_in$irrigGain
    avlWatWW            <- l_in$avlWatWW
    avlWatWC            <- l_in$avlWatWC

    IO_discharge        <- l_inout$discharge
    IO_minWatReserved   <- l_inout$minWatReserved
    IO_fracFullirrig    <- l_inout$fracFullirrig

    for (o in 1:max(rs$calcorder)) {
      cells <- which(rs$calcorder == o)

      for (c in cells) {
        ### Potential Function Improvements:
        # (1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS
        # (2) Allocation of certain share in first round (e.g. 75%); then filling up in second round if water left

        # Helper vectors for subsetting of objects
        # vector of downstreamcells of c
        v_down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        v_cell <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1s in length of downstreamcells of c
        v_ones <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        is_gain <- (I_irrigGain[c, y, , drop = FALSE] > l_in$gainthreshold)

        # available water for additional irrigation withdrawals
        avlWatWW[c, y, ][is_gain[, , , drop = F]] <- pmax(IO_discharge[c, y, , drop = F] -
                                                              IO_minWatReserved[c, y, , drop = F],
                                                            0)[is_gain[, , , drop = F]]

        # withdrawal constraint (if there is water required for withdrawal in current grid cell)
        is_req_ww   <- (I_reqWatFullirrigWW[c, y, , drop = F] > 0 & is_gain[, , , drop = F])

        # how much withdrawals can be fulfilled by available water
        IO_fracFullirrig[c, y, ][is_req_ww[, , , drop = F]] <- pmin(avlWatWW[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                                       I_reqWatFullirrigWW[c, y, , drop = F][is_req_ww[, , , drop = F]],
                                                                     1)

        if (length(v_down) > 0) {
          # consumption constraint (if there is water required for consumption in current grid cell)
          is_req_wc <- (I_reqWatFullirrigWC[c, y, , drop = F] > 0 & is_req_ww[, , , drop = F])

          # available water for additional irrigation consumption (considering downstream availability)
          avlWatWC[c, y, ][is_req_wc[, , , drop = F]]     <- pmax(apply(IO_discharge[v_down, y, , drop = F] -
                                                                            IO_minWatReserved[v_down, y, , drop = F], MARGIN = 3, min),
                                                                    0)[is_req_wc[, , , drop = F]]

          # how much consumption can be fulfilled by available water
          IO_fracFullirrig[c, y, ][is_req_wc[, , , drop = F]] <- pmin(avlWatWC[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                         I_reqWatFullirrigWC[c, y, , drop = F][is_req_wc[, , , drop = F]],
                                                                       IO_fracFullirrig[c, y, , drop = F][is_req_wc[, , , drop = F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        IO_discharge[c(v_down, c), y, ][is_req_ww[c(v_ones, 1), , , drop = F]] <- (IO_discharge[c(v_down, c), y, , drop = F] -
                                                                                     I_reqWatFullirrigWC[c(v_cell, c), y, , drop = F] * IO_fracFullirrig[c(v_cell, c), y, , drop = F])[is_req_ww[c(v_ones, 1), , , drop = F]]
        # update minimum water required in cell:
        IO_minWatReserved[c, y, ][is_req_ww[, , , drop = F]] <- (IO_minWatReserved[c, y, , drop = F] +
                                                                  IO_fracFullirrig[c, y, , drop = F] * I_reqWatFullirrigWW[c, y, , drop = F])[is_req_ww[, , , drop = F]]
      }

      l_inout <- list(discharge = IO_discharge,
                      minWatReserved = IO_minWatReserved,
                      fracFullirrig = IO_fracFullirrig)

    }
  }

  return(l_inout)
}
