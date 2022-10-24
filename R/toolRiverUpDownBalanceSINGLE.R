#' @title       toolRiverUpDownBalanceSINGLE
#' @description This function calculates the cell water balance
#'              under consideration of different reserved human uses
#'              (non-agricultural, neighbor water requirements,
#'               committed-agricultural uses)
#'
#' @param c              Current cell for which water shall be allocated
#' @param rs             River structure with information on upstreamcells,
#'                       downstreamcells and neighboring cells and distances
#' @param inLIST         List of objects that are inputs to the function:
#'                       yearly runoff in current cell; lake evaporation in current cell;
#'                       previously reservered withdrawal in all cells;
#'                       previously reserved consumption in current cell
#' @param inoutLIST      List of objects that are inputs to the function and
#'                       are updated by the function:
#'                       discharge, inflow,
#'                       currently requested flows (withdrawal and consumption)
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames new.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return list of arrays objects in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#' @export

toolRiverUpDownBalanceSINGLE <- function(c, rs, inLIST, inoutLIST) {

  # vector of upstreamcells of c
  up   <- unlist(rs$upstreamcells[[c]])
  # vector of downstreamcells of c
  down <- unlist(rs$downstreamcells[[c]])

  # Inputs (not altered in this algorithm):
  runoffWOEvap       <- inLIST$runoffEvap
  prevWW             <- inLIST$prevReservedWW
  prevWC             <- inLIST$prevReservedWC
  currRequestWWlocal <- inLIST$currRequestWWlocal

  # Inputs that are also outputs (updated by this algorithm):
  discharge          <- inoutLIST$discharge
  inflow             <- inoutLIST$inflow
  currRequestWClocal <- inoutLIST$currRequestWClocal

  # Helper variable for downstream accounting
  dischargeOLD       <- discharge[c]

  ############################################
  ### Upstream-Downstream Water Accounting ###
  ###         and flow reservation         ###
  ############################################

  # Available water in cell
  avlWat <- inflow[c] + runoffWOEvap

  #### (1) Available Water in cell is sufficient to fulfill previously determined requirements ####
  ####      -> further withdrawals possible                                                    ####
  # Is there sufficient water available to fulfill previously determined requirements?
  if (avlWat >= prevWW[c]) {
    # Are current withdrawals requested?
    if (currRequestWWlocal[c] > 0) {
      # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
      #                                  local previously determined water requirements are served
      fracFulfilled <- min((avlWat - prevWW[c]) / currRequestWWlocal[c],
                              1)

      # Current water uses (withdrawals and consumption) given withdrawal constraint
      currRequestWClocal[c] <- fracFulfilled * currRequestWClocal[c]
      #currRequestWWlocal[c] <- fracFulfilled * currRequestWWlocal[c]
    }

    # Discharge in current cell for case where sufficient water available for requirements
    # (Subtract local water consumption in current cell (and previous if applicable)
    discharge[c] <- (avlWat - currRequestWClocal[c] - prevWC)
    
  } else {

    #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
    ####      -> no more water can be withdrawn locally (A + B)                                      ####
    ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
    ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

    # No local water consumption if available water is not sufficient to fulfill
    # previously determined requirements
    currRequestWClocal[c] <- 0
  
    # Update upstream cells' current consumption:
    # (Note: This is necessary to allocate the release of water
    #        equally to all upstream cells (considering all
    #        tributaries and all cells in each of them))
    if (length(up) > 0) {

      # Determine upstream current water consumption:
      upstreamWC <- sum(currRequestWClocal[up])

      ### (A or B) Is there current upstream water consumption that can be reduced  ###
      ###          to release water required by previous (prioritary) uses?         ###
      if (upstreamWC > (prevWW[c] - avlWat)) {

        ## (A) upstreamWC high enough to release required water: ##
        ## -> reduce upstream consumption respectively           ##
        # (II) Water consumption constraint: If water is required by priority
        #                                    use in downstream cell, cannot have
        #                                    been consumed for current use upstream
        # fraction that stays in upstream cell(s) = 1 - fraction of water that
        #                                               needs to be released by each upstream cell
        fracFulfilled <- (1 - (prevWW[c] - avlWat) / upstreamWC)

        # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
        currRequestWClocal[up] <- fracFulfilled * currRequestWClocal[up]
        #currRequestWWlocal[up] <- fracFulfilled * currRequestWWlocal[up]

        # Discharge in current cell when water not sufficient to fulfill requirements,
        # but missing water water requirements can be fulfilled by upstream cells (A)
        discharge[c] <- prevWW[c] - prevWC

      } else {
        ## (B) upstreamWC not high enough to release required water:
        ## -> no water can be used upstream
        # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
        currRequestWClocal[up] <- 0
        #currRequestWWlocal[up] <- 0

        # Discharge in current cell when water not sufficient to fulfill requirements
        # and missing water water requirements cannot be fulfilled by upstream cells
        # (since there is no upstream consumption, this water is additionally available in the current cell)
        discharge[c] <- (avlWat + upstreamWC - prevWC)
      }
    } else {
      # Discharge when water is not sufficient to fulfill previously (priority)
      # requirements and there are no upstream cells
      discharge[c]  <- (avlWat - prevWC)
    }
  }

  # Update inflow and discharge in all downstream cells
  if (length(down) > 0) {
    discharge[down] <- discharge[down] + (discharge[c] - dischargeOLD)
    inflow[down] <- inflow[down] + discharge[down]
  }

  outLIST <- list(discharge = discharge,
                  inflow = inflow,
                  currRequestWClocal = currRequestWClocal)

  return(outLIST)

}