#' @title       toolRiverUpDownBalance
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
#' @return list of magpie objects in cellular resolution
#' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
#'
#' @export

toolRiverUpDownBalance <- function(c, rs, inLIST, inoutLIST) {

  # Inputs (not altered in this algorithm):
  runoffEvap     <- inLIST$runoffEvap
  prevReservedWW <- inLIST$prevReservedWW
  prevReservedWC <- inLIST$prevReservedWC
  currRequestWWlocal <- inLIST$currRequestWWlocal

  # Inputs that are also outputs (updated by this algorithm):
  discharge          <- inoutLIST$discharge
  inflow             <- inoutLIST$inflow
  currRequestWClocal <- inoutLIST$currRequestWClocal

  # Initialize function-internal variables with correct dimension:
  avlWat <- upstreamWC <- discharge
  avlWat[, , ]     <- 0
  upstreamWC[, , ] <- 0

  dischargeOLD <- discharge[c, , , drop = FALSE]

  # vector of downstreamcells of c
  down <- unlist(rs$downstreamcells[[c]])
  vCELL <- rep(c, length(rs$downstreamcells[[c]]))

  ############################################
  ### Upstream-Downstream Water Accounting ###
  ###         and flow reservation         ###
  ############################################

  # Available water in cell
  avlWat[c, , ] <- inflow[c, , , drop = FALSE] +
                      runoffEvap

  ### Is there sufficient water available to fulfill previously determined requirements? ###
  sufficientWat <- (avlWat[c, , , drop = FALSE] >= prevReservedWW[c, , , drop = FALSE])

  #### (1) Available Water in cell is sufficient to fulfill previously determined requirements ####
  ####      -> further withdrawals possible                                                    ####

  # current withdrawals requested?
  isWWreq  <- (currRequestWWlocal[c, , , drop = FALSE] > 0)
  # combined conditions
  isSuffWW <- sufficientWat & isWWreq

  # (I) Water withdrawal constraint: All withdrawals that can be fulfilled considering
  #                                  local previously determined water requirements are served
  fracFulfilled <- pmin((avlWat[c, , , drop = FALSE] -
                              prevReservedWW[c, , , drop = FALSE])[isSuffWW] /
                                currRequestWWlocal[c, , , drop = FALSE][isSuffWW],
                              1)
  # Current water uses (withdrawals and consumption) given withdrawal constraint
  currRequestWClocal[c, , ][isSuffWW] <- fracFulfilled * (currRequestWClocal[c, , ,
                                                                  drop = FALSE])[isSuffWW]
  currRequestWWlocal[c, , ][isSuffWW] <- fracFulfilled * (currRequestWWlocal[c, , , # OPTIMIZE: do only in the very end
                                                                  drop = FALSE])[isSuffWW]

  # Discharge in current cell for case where sufficient water available for requirements
  # (Subtract local water consumption in current cell (and previous if applicable)
  discharge[c, , ][sufficientWat] <- (avlWat[c, , , drop = FALSE] -
                                          currRequestWClocal[c, , , drop = FALSE] -
                                            prevReservedWC)[sufficientWat]

  #### (2) Available Water in cell is not sufficient to fulfill previously determined requirements ####
  ####      -> no more water can be withdrawn locally (A + B)                                      ####
  ####      &  if possible: upstream consumption is reduced to release missing water    (A)        ####
  ####         if not: nothing can be consumed upstream (upstream consumption set to 0) (B)        ####

  # No water withdrawals locally if available water is not sufficient to fulfill
  # previously determined requirements
  currRequestWClocal[c, , ][!sufficientWat] <- 0
  currRequestWWlocal[c, , ][!sufficientWat] <- 0

  # Update upstream cells' current consumption:
  if (length(rs$upstreamcells[[c]]) > 0) {

    # vector of upstreamcells of c
    up <- unlist(rs$upstreamcells[[c]])
    # vector of c in length of upstreamcells of c
    lc <- rep(c, length(rs$upstreamcells[[c]]))
    # vector of 1 in length of upstreamcells of c
    cc <- rep(1, length(rs$upstreamcells[[c]]))

    # Determine upstream current water consumption:
    upstreamWC[c, , ]  <- colSums(currRequestWClocal[up, , , drop = FALSE],
                                  dims = 1)

    ### (A or B) Is there current upstream water consumption that can be reduced  ###
    ###          to release water required by previous (prioritary) uses?         ###
    sufficientUP <- (upstreamWC[c, , , drop = FALSE] > (prevReservedWW[c, , , drop = FALSE] -
                                                          avlWat[c, , , drop = FALSE]))
    # Combinations of conditions
    fromUPcc <- (!sufficientWat[cc, , , drop = FALSE] & sufficientUP[cc, , , drop = FALSE])
    fromUPc  <- (!sufficientWat & sufficientUP)
    insuffWATcc <- (!sufficientWat[cc, , , drop = FALSE] & !sufficientUP[cc, , , drop = FALSE])
    insuffWATc  <- (!sufficientWat & !sufficientUP)

    ## (A) upstreamWC high enough to release required water: ##
    ## -> reduce upstream consumption respectively           ##
    # (II) Water consumption constraint: If water is required by priority
    #                                    use in downstream cell, cannot have
    #                                    been consumed for current use upstream
    # fraction that stays in upstream cell(s) = 1 - fraction of water that
    #                                               needs to be released by each upstream cell
    fracFulfilled <- (1 - (prevReservedWW[lc, , , drop = FALSE] -
                                  avlWat[lc, , , drop = FALSE])[fromUPcc] /
                                    upstreamWC[lc, , , drop = FALSE][fromUPcc])

    # Current human uses (consumption and withdrawal) in upstreamcells is reduced by respective amount
    currRequestWClocal[up, , ][fromUPcc] <- fracFulfilled * (currRequestWClocal[up, , , drop = FALSE])[fromUPcc]
    currRequestWWlocal[up, , ][fromUPcc] <- fracFulfilled * (currRequestWWlocal[up, , , drop = FALSE])[fromUPcc]

    # Discharge in current cell when water not sufficient to fulfill requirements,
    # but missing water water requirements can be fulfilled by upstream cells (A)
    discharge[c, , ][fromUPc]  <-  (prevReservedWW[c, , , drop = FALSE] -
                                      prevReservedWC)[fromUPc]

    ## (B) upstreamWC not high enough to release required water:
    ## -> no water can be used upstream
    # Current human uses (consumption and withdrawal) in upstreamcells are set to 0
    currRequestWClocal[up, , ][insuffWATcc] <- 0
    currRequestWWlocal[up, , ][insuffWATcc] <- 0

    # Discharge in current cell when water not sufficient to fulfill requirements
    # and missing water water requirements cannot be fulfilled by upstream cells
    # (since there is no upstream consumption, this water is additionally available in the current cell)
    discharge[c, , ][insuffWATc] <- (avlWat[c, , , drop = FALSE] +
                                        upstreamWC[c, , , drop = FALSE] -
                                          prevReservedWC)[insuffWATc]

  } else {

    # Discharge when water is not sufficient to fulfill previously (priority)
    # requirements and there are no upstream cells
    discharge[c, , ][!sufficientWat]  <- (avlWat[c, , , drop = FALSE] -
                                              prevReservedWC)[!sufficientWat]

  }

  # Update inflow and discharge in all downstream cells
  if (length(down) > 0) {
    discharge[down, , ] <- discharge[down, , , drop = FALSE] + 
                            (discharge[vCELL, , , drop = FALSE] - 
                               dischargeOLD[vCELL, , , drop = FALSE])
    inflow[down, , ] <- inflow[down, , , drop = FALSE] + discharge[down, , , drop = FALSE]
  }

  outLIST <- list(discharge = discharge,
                  inflow = inflow,
                  currRequestWClocal = currRequestWClocal,
                  frac = fracFulfilled)

  return(outLIST)

}