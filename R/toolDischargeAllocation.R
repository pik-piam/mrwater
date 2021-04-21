#' @title       toolDischargeAllocation
#' @description This tool function executes the Allocation Algorithm in cell order of selected allocation rule
#'
#' @param y Current year of loop
#' @param rs River structure (list of river structure details and cell numbers including ordered cell number)
#' @param l_inout list of inputs that are at the same time outputs: required_wat_min_allocation Minimum (water requirement reserved per grid cell (as magpie object of correct dimension)); discharge (Discharge to be allocated (as magpie object of correct dimension)); frac_fullirrig (fraction of fullirrigation requirements that can be fulfilled)
#' @param l_in list of inputs: irrig_yieldgainpotential (yield gain potential through irrigation of proxy crops: magpie object with cellular and year dimension (as magpie object of correct dimension)); required_wat_fullirrig_ww (required withdrawal for full irrigation in specific cell (as magpie object of correct dimension)); required_wat_fullirrig_wc (required consumption for full irrigation in specific cell (as magpie object of correct dimension)); gainthreshold (Minimum yield gain in USD/ha (as scalar value)); avl_wat_ww; avl_wat_wc
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param glocellrank cell ranking for different years (array). Note: only applicable when allocationrule "optimization" chosen
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'

toolDischargeAllocation <- function(y, rs, l_inout, l_in, allocationrule, glocellrank) {

  # Cell ordering to be applied for surplus discharge allocation rules
  if (allocationrule=="optimization") {

    # Retrieve arguments
    reducedpotential               <- any(grepl("A_", getCells(glocellrank)))
    I_required_wat_fullirrig_ww    <- l_in$required_wat_fullirrig_ww
    I_required_wat_fullirrig_wc    <- l_in$required_wat_fullirrig_wc
    I_irrig_yieldgainpotential     <- l_in$irrig_yieldgainpotential
    avl_wat_ww                     <- l_in$avl_wat_ww
    avl_wat_wc                     <- l_in$avl_wat_wc
    inaccessible_discharge         <- l_in$inaccessible_discharge
    com_ww                         <- l_in$com_ww

    # Share of full irrigation water requirements to be allocated for each round of the allocation algorithm
    allocationshare             <- 1 / (length(glocellrank[,1])/67420)
    I_required_wat_fullirrig_ww <- I_required_wat_fullirrig_ww * allocationshare
    I_required_wat_fullirrig_wc <- I_required_wat_fullirrig_wc * allocationshare

    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    for (o in (1:max(glocellrank[,y], na.rm=T))) { #test <- rs$cells[order(glocellrank[,y])]

      # Extract the cell number (depending on type of cellranking)
      if (reducedpotential) {
        c <- rs$cells[rs$coordinates==paste(strsplit(gsub(".*_", "", names(which(glocellrank[,y]==o))), "\\.")[[1]][1], strsplit(gsub(".*_", "", names(which(glocellrank[,y]==o))), "\\.")[[1]][2], sep=".")]
      } else {
        c <- rs$cells[glocellrank[,y]==o]
      }

      ### Potential Function Improvements:
      #(1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS
      #(2) Allocation of certain share in first round (e.g. 75%); then filling up in second round if water left

      IO_discharge                   <- l_inout$discharge
      IO_required_wat_min_allocation <- l_inout$required_wat_min_allocation
      IO_frac_fullirrig              <- l_inout$frac_fullirrig

      # Helper vectors for subsetting of objects
      # vector of downstreamcells of c
      v_down <- unlist(rs$downstreamcells[[c]])
      # vector of c in length of downstreamcells of c
      v_cell <- rep(c, length(rs$downstreamcells[[c]]))
      # vector of 1s in length of downstreamcells of c
      v_ones <- rep(1:length(c), length(rs$downstreamcells[[c]]))

      # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
      is_gain <- (I_irrig_yieldgainpotential[c,y,,drop=F] > l_in$gainthreshold)

      # available water for additional irrigation withdrawals
      # (Avl. water = Discharge - Inaccessible Discharge - Previously Committed for Human Consumption)
      # (Inaccessible Discharge is either reserved for the environment: EFRs (required_wat_min_allocation - com_ww) or inaccessible due to variability (inaccessible_discharge) needs to be reserved)
      avl_wat_ww[c,y,][is_gain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - pmax(IO_required_wat_min_allocation[c,y,,drop=F] - com_ww[c,y,,drop=F], inaccessible_discharge[c,y,,drop=F]) - com_ww[c,y,,drop=F], 0)[is_gain[,,,drop=F]]

      # withdrawal constraint (if there is water required for withdrawal in current grid cell)
      is_req_ww   <- (I_required_wat_fullirrig_ww[c,y,,drop=F]>0 & is_gain[,,,drop=F])

      # how much withdrawals can be fulfilled by available water
      IO_frac_fullirrig[c,y,][is_req_ww[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][is_req_ww[,,,drop=F]] / I_required_wat_fullirrig_ww[c,y,,drop=F][is_req_ww[,,,drop=F]], 1)

      if (length(v_down)>0) {
        # consumption constraint (if there is water required for consumption in current grid cell)
        is_req_wc <- (I_required_wat_fullirrig_wc[c,y,,drop=F]>0 & is_req_ww[,,,drop=F])

        # available water for additional irrigation consumption (considering downstream availability)
        # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
        avl_wat_wc[c,y,][is_req_wc[,,,drop=F]]     <- pmax(apply((pmax(IO_discharge[v_down,y,,drop=F] - pmax(IO_required_wat_min_allocation[v_down,y,,drop=F] - com_ww[v_down,y,,drop=F], inaccessible_discharge[v_down,y,,drop=F]) - com_ww[v_down,y,,drop=F], 0)), MARGIN=3, min)[is_req_wc[,,,drop=F]], 0)

        # how much consumption can be fulfilled by available water
        IO_frac_fullirrig[c,y,][is_req_wc[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][is_req_wc[,,,drop=F]] / I_required_wat_fullirrig_wc[c,y,,drop=F][is_req_wc[,,,drop=F]], IO_frac_fullirrig[c,y,,drop=F][is_req_wc[,,,drop=F]])
      }

      # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
      IO_discharge[c(v_down,c),y,][is_req_ww[c(v_ones,1),,,drop=F]] <- (IO_discharge[c(v_down,c),y,,drop=F] - I_required_wat_fullirrig_wc[c(v_cell,c),y,,drop=F] * IO_frac_fullirrig[c(v_cell,c),y,,drop=F])[is_req_ww[c(v_ones,1),,,drop=F]]
      # update minimum water required in cell:
      IO_required_wat_min_allocation[c,y,][is_req_ww[,,,drop=F]]    <- (IO_required_wat_min_allocation[c,y,,drop=F] + IO_frac_fullirrig[c,y,,drop=F] * I_required_wat_fullirrig_ww[c,y,,drop=F])[is_req_ww[,,,drop=F]]
      com_ww[c,y,][is_req_ww[,,,drop=F]]                            <- (com_ww[c,y,,drop=F] + IO_frac_fullirrig[c,y,,drop=F] * I_required_wat_fullirrig_ww[c,y,,drop=F])[is_req_ww[,,,drop=F]]

      # Function output
      discharge                   <- IO_discharge
      required_wat_min_allocation <- IO_required_wat_min_allocation
      frac_fullirrig              <- IO_frac_fullirrig

      l_inout <- list(discharge=discharge, required_wat_min_allocation=required_wat_min_allocation, frac_fullirrig=frac_fullirrig)

    }

  } else if (allocationrule=="upstreamfirst") {

    I_required_wat_fullirrig_ww    <- l_in$required_wat_fullirrig_ww
    I_required_wat_fullirrig_wc    <- l_in$required_wat_fullirrig_wc
    I_irrig_yieldgainpotential     <- l_in$irrig_yieldgainpotential
    avl_wat_ww                     <- l_in$avl_wat_ww
    avl_wat_wc                     <- l_in$avl_wat_wc
    inaccessible_discharge         <- l_in$inaccessible_discharge
    com_ww                         <- l_in$com_ww

    for (o in 1:max(rs$calcorder)) {
      cells <- which(rs$calcorder==o)

      for (c in cells){
        ### Potential Function Improvements:
        #(1) GENERALIZE: FLEXIBLE FOR YEARS AND CELLS
        #(2) Allocation of certain share in first round (e.g. 75%); then filling up in second round if water left

        IO_discharge                   <- l_inout$discharge
        IO_required_wat_min_allocation <- l_inout$required_wat_min_allocation
        IO_frac_fullirrig              <- l_inout$frac_fullirrig

        # Helper vectors for subsetting of objects
        # vector of downstreamcells of c
        v_down <- unlist(rs$downstreamcells[[c]])
        # vector of c in length of downstreamcells of c
        v_cell <- rep(c, length(rs$downstreamcells[[c]]))
        # vector of 1s in length of downstreamcells of c
        v_ones <- rep(1:length(c), length(rs$downstreamcells[[c]]))

        # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
        is_gain <- (I_irrig_yieldgainpotential[c,y,,drop=F] > l_in$gainthreshold)

        # available water for additional irrigation withdrawals
        # (Avl. water = Discharge - Inaccessible Discharge - Previously Committed for Human Consumption)
        # (Inaccessible Discharge is either reserved for the environment: EFRs (required_wat_min_allocation - com_ww) or inaccessible due to variability (inaccessible_discharge) needs to be reserved)
        avl_wat_ww[c,y,][is_gain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - pmax(IO_required_wat_min_allocation[c,y,,drop=F] - com_ww[c,y,,drop=F], inaccessible_discharge[c,y,,drop=F]) - com_ww[c,y,,drop=F], 0)[is_gain[,,,drop=F]]

        # withdrawal constraint (if there is water required for withdrawal in current grid cell)
        is_req_ww   <- (I_required_wat_fullirrig_ww[c,y,,drop=F]>0 & is_gain[,,,drop=F])

        # how much withdrawals can be fulfilled by available water
        IO_frac_fullirrig[c,y,][is_req_ww[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][is_req_ww[,,,drop=F]] / I_required_wat_fullirrig_ww[c,y,,drop=F][is_req_ww[,,,drop=F]], 1)

        if (length(v_down)>0) {
          # consumption constraint (if there is water required for consumption in current grid cell)
          is_req_wc <- (I_required_wat_fullirrig_wc[c,y,,drop=F]>0 & is_req_ww[,,,drop=F])

          # available water for additional irrigation consumption (considering downstream availability)
          # (downstream availability is constrained by EFRs and inaccessible discharge just as local withdrawal constraint above)
          avl_wat_wc[c,y,][is_req_wc[,,,drop=F]]     <- pmax(apply((pmax(IO_discharge[v_down,y,,drop=F] - pmax(IO_required_wat_min_allocation[v_down,y,,drop=F] - com_ww[v_down,y,,drop=F], inaccessible_discharge[v_down,y,,drop=F]) - com_ww[v_down,y,,drop=F], 0)), MARGIN=3, min)[is_req_wc[,,,drop=F]], 0)

          # how much consumption can be fulfilled by available water
          IO_frac_fullirrig[c,y,][is_req_wc[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][is_req_wc[,,,drop=F]] / I_required_wat_fullirrig_wc[c,y,,drop=F][is_req_wc[,,,drop=F]], IO_frac_fullirrig[c,y,,drop=F][is_req_wc[,,,drop=F]])
        }

        # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
        IO_discharge[c(v_down,c),y,][is_req_ww[c(v_ones,1),,,drop=F]] <- (IO_discharge[c(v_down,c),y,,drop=F] - I_required_wat_fullirrig_wc[c(v_cell,c),y,,drop=F] * IO_frac_fullirrig[c(v_cell,c),y,,drop=F])[is_req_ww[c(v_ones,1),,,drop=F]]
        # update minimum water required in cell:
        IO_required_wat_min_allocation[c,y,][is_req_ww[,,,drop=F]]    <- (IO_required_wat_min_allocation[c,y,,drop=F] + IO_frac_fullirrig[c,y,,drop=F] * I_required_wat_fullirrig_ww[c,y,,drop=F])[is_req_ww[,,,drop=F]]
        com_ww[c,y,][is_req_ww[,,,drop=F]]                            <- (com_ww[c,y,,drop=F] + IO_frac_fullirrig[c,y,,drop=F] * I_required_wat_fullirrig_ww[c,y,,drop=F])[is_req_ww[,,,drop=F]]

        # Function output
        discharge                   <- IO_discharge
        required_wat_min_allocation <- IO_required_wat_min_allocation
        frac_fullirrig              <- IO_frac_fullirrig

        l_inout <- list(discharge=discharge, required_wat_min_allocation=required_wat_min_allocation, frac_fullirrig=frac_fullirrig)

      }
    }
  }

  return(l_inout)
}
