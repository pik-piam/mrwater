#' @title       toolDischargeAllocation
#' @description This tool function executes the Allocation Algorithm in
#'              cell order of selected allocation rule
#'
#' @param y              Current year of loop
#' @param rs             River structure (list of river structure details and
#'                       cell numbers including ordered cell number)
#' @param l_inout        List of inputs that are at the same time outputs:
#'                       required_wat_min_allocation Minimum (water requirement reserved per grid cell (as magpie object of correct dimension));
#'                       discharge (Discharge to be allocated (as magpie object of correct dimension));
#'                       frac_fullirrig (fraction of fullirrigation requirements that can be fulfilled)
#' @param l_in           List of inputs:
#'                       irrig_yieldgainpotential (yield gain potential through irrigation of proxy crops: magpie object with cellular and year dimension (as magpie object of correct dimension));
#'                       required_wat_fullirrig_ww (required withdrawal for full irrigation in specific cell (as magpie object of correct dimension));
#'                       required_wat_fullirrig_wc (required consumption for full irrigation in specific cell (as magpie object of correct dimension));
#'                       gainthreshold (Minimum yield gain in USD/ha (as scalar value)); avl_wat_ww; avl_wat_wc
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
    I_required_wat_fullirrig_ww    <- l_in$required_wat_fullirrig_ww
    I_required_wat_fullirrig_wc    <- l_in$required_wat_fullirrig_wc
    I_irrig_yieldgainpotential     <- l_in$irrig_yieldgainpotential
    avl_wat_ww                     <- l_in$avl_wat_ww
    avl_wat_wc                     <- l_in$avl_wat_wc
    scenarios                      <- l_in$scenarios

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    if (l_in$multicropping) {

      # allocation share needs to be adjusted for multicropping length
      tmp <- calcOutput("MultipleCroppingZones", layers = 3, aggregate = FALSE)
      tmp <- range(tmp)[2]

      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / tmp / 67420)

    } else {

      # allocation share depends on chosen cellranking
      allocationshare           <- 1 / (length(glocellrank[, 1, 1]) / 67420)

    }
    I_required_wat_fullirrig_ww <- I_required_wat_fullirrig_ww * allocationshare
    I_required_wat_fullirrig_wc <- I_required_wat_fullirrig_wc * allocationshare

    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    IO_discharge                   <- l_inout$discharge
    IO_required_wat_min_allocation <- l_inout$required_wat_min_allocation
    IO_frac_fullirrig              <- l_inout$frac_fullirrig

    for (o in (1:max(glocellrank[, y, ], na.rm = T))) { # test <- rs$cells[order(glocellrank[,y,])]

      # Extract the cell number
      c <- rs$cells[rs$coordinates == paste(strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][1],
                                            strsplit(gsub(".*_", "", names(which(glocellrank[, y, ] == o))), "\\.")[[1]][2],
                                            sep = ".")]

      # Extract season
      if (l_in$multicropping) {

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
      is_gain <- (I_irrig_yieldgainpotential[c, y, season, drop = F] > l_in$gainthreshold)

      # available water for additional irrigation withdrawals
      avl_wat_ww[c, y, ][is_gain[, , , drop = F]] <- pmax(IO_discharge[c, y, , drop = F] -
                                                     IO_required_wat_min_allocation[c, y, , drop = F],
                                                   0)[is_gain[, , , drop = F]]

      # withdrawal constraint (if there is water required for withdrawal in current grid cell)
      is_req_ww   <- (I_required_wat_fullirrig_ww[c, y, season, drop = F] > 0 & is_gain[, , , drop = F])

      # how much withdrawals can be fulfilled by available water
      IO_frac_fullirrig[c, y, ][is_req_ww[, , , drop = F]] <- pmin(avl_wat_ww[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                              I_required_wat_fullirrig_ww[c, y, season, drop = F][is_req_ww[, , , drop = F]],
                                                            1)

      if (length(v_down) > 0) {
        # consumption constraint (if there is water required for consumption in current grid cell)
        is_req_wc <- (I_required_wat_fullirrig_wc[c, y, season, drop = F] > 0 & is_req_ww[, , , drop = F])

        # available water for additional irrigation consumption (considering downstream availability)
        avl_wat_wc[c, y, ][is_req_wc[, , , drop = F]] <- pmax(apply(IO_discharge[v_down, y, , drop = F] -
                                                                    IO_required_wat_min_allocation[v_down, y, , drop = F], MARGIN = 3, min),
                                                              0)[is_req_wc[, , , drop = F]]

        # how much consumption can be fulfilled by available water
        IO_frac_fullirrig[c, y, ][is_req_wc[, , , drop = F]] <- pmin(avl_wat_wc[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                       I_required_wat_fullirrig_wc[c, y, season, drop = F][is_req_wc[, , , drop = F]],
                                                                     IO_frac_fullirrig[c, y, , drop = F][is_req_wc[, , , drop = F]])
      }

      # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
      IO_discharge[c(v_down, c), y, ][is_req_ww[c(v_ones, 1), , , drop = F]] <- (IO_discharge[c(v_down, c), y, , drop = F] -
                                                                                 I_required_wat_fullirrig_wc[c(v_cell, c), y, season, drop = F] * IO_frac_fullirrig[c(v_cell, c), y, , drop = F])[is_req_ww[c(v_ones, 1), , , drop = F]]
      # update minimum water required in cell:
      IO_required_wat_min_allocation[c, y, ][is_req_ww[, , , drop = F]]      <- (IO_required_wat_min_allocation[c, y, , drop = F] +
                                                                                 IO_frac_fullirrig[c, y, , drop = F] * I_required_wat_fullirrig_ww[c, y, season, drop = F])[is_req_ww[, , , drop = F]]
    }

    l_inout <- list(discharge = IO_discharge,
                    required_wat_min_allocation = IO_required_wat_min_allocation,
                    frac_fullirrig = IO_frac_fullirrig)

  } else if (allocationrule == "upstreamfirst") {

    I_required_wat_fullirrig_ww    <- l_in$required_wat_fullirrig_ww
    I_required_wat_fullirrig_wc    <- l_in$required_wat_fullirrig_wc
    I_irrig_yieldgainpotential     <- l_in$irrig_yieldgainpotential
    avl_wat_ww                     <- l_in$avl_wat_ww
    avl_wat_wc                     <- l_in$avl_wat_wc
    scenarios                      <- l_in$scenarios

    IO_discharge                   <- l_inout$discharge
    IO_required_wat_min_allocation <- l_inout$required_wat_min_allocation
    IO_frac_fullirrig              <- l_inout$frac_fullirrig


    # Allocate first, then second, then third season
    for (season in c("single", "double", "triple")) {

      season <- paste(scenarios, "single", sep = ".")

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
          is_gain <- (I_irrig_yieldgainpotential[c, y, season, drop = F] > l_in$gainthreshold)

          # available water for additional irrigation withdrawals
          avl_wat_ww[c, y, ][is_gain[, , , drop = F]] <- pmax(IO_discharge[c, y, , drop = F] -
                                                                IO_required_wat_min_allocation[c, y, , drop = F],
                                                              0)[is_gain[, , , drop = F]]

          # withdrawal constraint (if there is water required for withdrawal in current grid cell)
          is_req_ww   <- (I_required_wat_fullirrig_ww[c, y, season, drop = F] > 0 & is_gain[, , , drop = F])

          # how much withdrawals can be fulfilled by available water
          IO_frac_fullirrig[c, y, ][is_req_ww[, , , drop = F]] <- pmin(avl_wat_ww[c, y, , drop = F][is_req_ww[, , , drop = F]] /
                                                                         I_required_wat_fullirrig_ww[c, y, season, drop = F][is_req_ww[, , , drop = F]],
                                                                       1)

          if (length(v_down) > 0) {
            # consumption constraint (if there is water required for consumption in current grid cell)
            is_req_wc <- (I_required_wat_fullirrig_wc[c, y, season, drop = F] > 0 & is_req_ww[, , , drop = F])

            # available water for additional irrigation consumption (considering downstream availability)
            avl_wat_wc[c, y, ][is_req_wc[, , , drop = F]]     <- pmax(apply(IO_discharge[v_down, y, , drop = F] -
                                                                              IO_required_wat_min_allocation[v_down, y, , drop = F], MARGIN = 3, min),
                                                                      0)[is_req_wc[, , , drop = F]]

            # how much consumption can be fulfilled by available water
            IO_frac_fullirrig[c, y, ][is_req_wc[, , , drop = F]] <- pmin(avl_wat_wc[c, y, , drop = F][is_req_wc[, , , drop = F]] /
                                                                           I_required_wat_fullirrig_wc[c, y, season, drop = F][is_req_wc[, , , drop = F]],
                                                                         IO_frac_fullirrig[c, y, , drop = F][is_req_wc[, , , drop = F]])
          }

          # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
          IO_discharge[c(v_down, c), y, ][is_req_ww[c(v_ones, 1), , , drop = F]] <- (IO_discharge[c(v_down, c), y, , drop = F] -
                                                                                       I_required_wat_fullirrig_wc[c(v_cell, c), y, season, drop = F] * IO_frac_fullirrig[c(v_cell, c), y, , drop = F])[is_req_ww[c(v_ones, 1), , , drop = F]]
          # update minimum water required in cell:
          IO_required_wat_min_allocation[c, y, ][is_req_ww[, , , drop = F]]    <- (IO_required_wat_min_allocation[c, y, , drop = F] +
                                                                                     IO_frac_fullirrig[c, y, , drop = F] * I_required_wat_fullirrig_ww[c, y, season, drop = F])[is_req_ww[, , , drop = F]]
        }

        l_inout <- list(discharge = IO_discharge,
                        required_wat_min_allocation = IO_required_wat_min_allocation,
                        frac_fullirrig = IO_frac_fullirrig)
      }
    }
  }

  return(l_inout)
}
