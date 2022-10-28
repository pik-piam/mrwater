#' @title       toolRiverUpDownBalanceSINGLE
#' @description This function calculates the cell water balance
#'              under consideration of different reserved human uses
#'              (non-agricultural, neighbor water requirements,
#'               committed-agricultural uses)
#'
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

toolRiverUpDownBalanceSINGLE <- function(inLIST, inoutLIST) {

  # Inputs (not altered in this iteration of the algorithm):
  prevWW <- inLIST$prevWW
  currWW <- inLIST$currWW

  # Inputs that are also outputs (updated by this algorithm):
  q      <- inoutLIST$q
  currWC <- inoutLIST$currWC

  # Extract cell lists
  downCELLS <- NULL
  upCELLS   <- NULL
  if (length(currWC) > 1) {
    upCELLS <- seq(2, length(currWC), 1)
  }
  if (length(q) > 1) {
    downCELLS <- seq(2, length(q), 1)
    downCELLS <- setdiff(downCELLS, upCELLS)
  }
  # Current cell and downstreamcells (if exist)
  cells <- c(1, downCELLS)

  ############################################
  ### Upstream-Downstream Water Accounting ###
  ###         and flow reservation         ###
  ############################################

  #### (1) Available Water in cell is sufficient to fulfill previously determined requirements ####
  ####      -> further withdrawals possible                                                    ####
  # Is there sufficient water available to fulfill previously determined requirements?
  if ((q[1] - prevWW) > 0) {
    # Are current withdrawals requested?
    if (currWW > 0) {
      # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
      #                                  local previously determined water requirements are served
      frac <- min((q[1] - prevWW) / currWW,
                  1)

      # Current water uses fulfilled given withdrawal constraint
      currWC[1] <- frac * currWC[1]
    }

    # Update discharge in current cell and downstream cells
    # for case where sufficient water available for requirements
    # (Subtract local water consumption in current cell (and previous if applicable)
    q[cells] <- q[cells] - currWC[1]

  } else {

    #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
    ####      -> no more water can be withdrawn locally (A + B)                                      ####
    ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
    ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

    # No local water consumption if available water is not sufficient to fulfill
    # previously determined requirements
    currWC[1] <- 0

    # Update upstream cells' current consumption:
    # (Note: This is necessary to allocate the release of water
    #        equally to all upstream cells (considering all
    #        tributaries and all cells in each of them))
    if (length(currWC) > 1) {

      # Determine upstream current water consumption:
      upstreamWC <- sum(currWC[upCELLS])

      ### (A or B) Is there current upstream water consumption that can be reduced  ###
      ###          to release water required by previous (prioritary) uses?         ###
      if (upstreamWC > (prevWW - q[1])) {
        ## (A) upstreamWC high enough to release required water: ##
        ## -> reduce upstream consumption respectively           ##
        # (II) Water consumption constraint: If water is required by priority
        #                                    use in downstream cell, cannot have
        #                                    been consumed for current use upstream
        # fraction that stays in upstream cell(s) = 1 - fraction of water that
        #                                               needs to be released by each upstream cell
        frac <- (prevWW - q[1]) / upstreamWC

        # Update discharge of upstream cells, cell and downstream cells
        q[upCELLS] <- q[upCELLS] + frac * currWC[upCELLS]
        q[cells]   <- q[cells] + frac * upstreamWC
        # Reduce current human uses in upstreamcells
        currWC[upCELLS]  <- (1 - frac) * currWC[upCELLS]

      } else {
        ## (B) upstreamWC not high enough to release required water:
        ## -> no water can be used upstream
        # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
        # and discharge is reverted to previous amount
        q[upCELLS]      <- q[upCELLS] + currWC[upCELLS]
        currWC[upCELLS] <- 0

        # Update discharge in current cell and downstream cells
        # when water not sufficient to fulfill requirements
        # and missing water water requirements cannot be fulfilled by upstream cells
        # (since there is no upstream consumption, this water is additionally available in the current cell)
        q[cells] <- q[cells] + upstreamWC
      }
    }
  }

  outLIST <- list(q = q,
                  currWC = currWC)

  return(outLIST)
}
