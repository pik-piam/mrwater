# #' @title       toolRiverDischargeAllocation
# #' @description This tool function allocates discharge
# #'              for grid cells respecting upstream-downstream relationships
# #'              and various water constraints
# #'
# #' @param c              Current cell for which water shall be allocated
# #' @param rs             River structure with information on upstreamcells,
# #'                       downstreamcells and neighboring cells and distances
# #' @param iteration      Currently active iteration of river discharge allocation.
# #'                       Arguments:
# #'                       "main" for case of main river cells
# #'                       "neighbor" for case of neighboring cells of main river cells
# #' @param transDist      Water transport distance allowed to fulfill locally
# #'                       unfulfilled water demand by surrounding cell water availability
# #' @param inLIST         List of objects that are inputs to the function
# #' @param inoutLIST      List of objects that are inputs to the function and 
# #'                       are updated by the function 
# #'
# #' @return magpie object in cellular resolution
# #' @author Felicitas Beier, Jens Heinke, Jan Philipp Dietrich
# #'

# toolRiverDischargeAllocation <- function(c, rs, transDist, iteration, inLIST, inoutLIST) {

#   # Helper vectors for subsetting of objects
#   # vector of downstreamcells of c
#   vDOWN <- unlist(rs$downstreamcells[[c]])
#   # vector of c in length of downstreamcells of c
#   vCELL <- rep(c, length(rs$downstreamcells[[c]]))
#   # vector of 1s in length of downstreamcells of c
#   vONES <- rep(1, length(rs$downstreamcells[[c]]))

#   ###############################
#   ### Import required objects ###
#   ###############################
#   # Inputs
#   # gainthreshold, irrigGain

#   # Inputs that are also outputs 
#   # i.e. objects that are updated by this function)
  
#   # resNeighborWW, resNeighborWC
#   # neighborProvisionWW, neighborProvisionWC
#   # discharge, watReserved, 
#   # currHumanWWlocal, currHumanWClocal
#   # currHumanWWtotal, currHumanWCtotal

#   # missingWW, missingWC (only input or input and output? (for case of neighbor irrigation both?))

  
#   ##################
#   ### Conditions ###
#   ##################
#   # For Surplus Discharge Allocation:
#   # only cells where irrigation potential exceeds
#   # certain minimum threshold are (additionally) irrigated
#   isGAIN <- (inLIST$irrigGain[c, y, , drop = FALSE] > inLIST$gainthreshold)

#   # is water required for withdrawal in current grid cell?
#   isWW <- (reqWatFullirrigWW[c, y, , drop = FALSE] > 0 & isGAIN)

#   # is water required for consumption in current grid cell?
#   isWC <- (reqWatFullirrigWC[c, y, , drop = FALSE] > 0 & isWW)

#   ##########################
#   ### Internal Functions ###
#   ##########################
#   .determineAvailability <- function(dis, res) {
#     # available water for additional irrigation withdrawals
#     return(pmax(apply(dis - res, MARGIN = 3, min), 0))
#   }
#   .determineFractionFulfilled <- function(avl, req, frac) {
#     # how much withdrawals can be fulfilled by available water
#     return(pmin(avl / req, frac))
#   }
#   .updateUse <- function(wat, frac) {
#     return(wat * frac)
#   }
#   .updateReserved <- function(curUse, res) {
#     return(res + curUse)
#   }
#   .subtractFlows <- function(a, b) {
#     return(a - b)
#   }

#   ##########################
#   ###  Water Allocation  ###
#   ##########################

#   # Water Availability
#   avlWatWW[c, y, ][isGAIN] <- .determineAvailability(dis = dischargeNEW[c, y, , drop = FALSE],
#                                                      res = reservedNEW[c, y, , drop = FALSE])[isGAIN]

#   # Withdrawal Constraint
#   fracNEW[c, y, ][isWW] <- .determineFractionFulfilled(avl = avlWatWW[c, y, , drop = FALSE][isWW],
#                                                        req = reqWatFullirrigWW[c, y, , drop = FALSE][isWW],
#                                                        frac = 1)

#   if (length(vDOWN) > 0) {

#     # Water Availability (considering downstream availability)
#     avlWatWC[c, y, ][isWC] <- .determineAvailability(dis = dischargeNEW[vDOWN, y, , drop = FALSE],
#                                                      res = reservedNEW[vDOWN, y, , drop = FALSE])[isWC]

#     # Consumption Constraint
#     fracNEW[c, y, ][isWC] <- .determineFractionFulfilled(avl = avlWatWC[c, y, , drop = FALSE][isWC],
#                                                          req = reqWatFullirrigWC[c, y, , drop = FALSE][isWC],
#                                                          frac = fracNEW[c, y, , drop = FALSE][isWC])
#   }

#   # local irrigation water withdrawals and consumption
#   currHumanWClocal[c, y, ] <- .updateUse(wat  = reqWatFullirrigWC[c, y, , drop = FALSE],
#                                          frac = fracNEW[c, y, , drop = FALSE])
#   currHumanWWlocal[c, y, ] <- .updateUse(wat  = reqWatFullirrigWW[c, y, , drop = FALSE],
#                                          frac = fracNEW[c, y, , drop = FALSE])

#   # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#   dischargeNEW[c(vDOWN, c), y, ][isWW[c(vONES, 1), , ,
#                                  drop = FALSE]] <- .subtractFlows(a = dischargeNEW[c(vDOWN, c), y, , drop = FALSE],
#                                                                   b = currHumanWClocal[c(vCELL, c), y, , drop = FALSE]
#                                                                   )[isWW[c(vONES, 1), , , drop = FALSE]]
#   # update minimum water required in cell:
#   reservedNEW[c, y, ][isWW] <- .updateReserved(res = reservedNEW[c, y, , drop = FALSE],
#                                               curUse = currHumanWWlocal[c, y, , drop = FALSE])[isWW]



# #### CASE I: IF STANDARD ROUND (current cell c is cell of main routing)

# # Case 0: No neighbor irrigation
# #         Routing is already complete -> report all outputs and jumpt to next cell
# #         Once loop through all cells complete: finish
  
#   if (iteration == "main") {
#     # Neighbor Irrigation (under "optimization" scenario)
#     if (transDist != 0) {
#       if (allocationrule == "optimization") {
#         # Locally missing water that might be fulfilled by surrounding cells
#         missingWW <- .subtractFlows(a = reqWatFullirrigWW[c, y, , drop = FALSE],
#                                     b = currHumanWWlocal[c, y, , drop = FALSE])
#         missingWC <- .subtractFlows(a = reqWatFullirrigWC[c, y, , drop = FALSE],
#                                     b = currHumanWClocal[c, y, , drop = FALSE])
#         # Water Allocation in neighboring cells of c
#         tmp <- toolRiverDischargeAllocation(c = n, rs = rs, iteration = "neighbor")
#         ### update objects
#       }
#     }
#   } else if (iteration == "neighbor") {

#     # current neighbor use
#     currNeighborWW <- .updateUse(wat  = missingWW,
#                                  frac = fracNEW[c, , , drop = FALSE])
#     currNeighborWC <- .updateUse(wat  = missingWC,
#                                  frac = fracNEW[c, , , drop = FALSE])

#     # update reserved water in respective neighboring cell (current cell)
#     resNeighborWW[c, , ] <- .updateReserved(res    = resNeighborWW[c, , , drop = FALSE],
#                                             curUse = currNeighborWW)
#     resNeighborWC[c, , ] <- .updateReserved(res = resNeighborWC[c, , , drop = FALSE],
#                                             curUse = currNeighborWC)

#     # update water provided by neighboring cell to original cell that had missing water
#     neighborProvisionWW[k, , ] <- .updateReserved(res    = neighborProvisionWW[k, , , drop = FALSE],
#                                                   curUse = currNeighborWW)
#     neighborProvisionWC[k, , ] <- .updateReserved(res    = neighborProvisionWC[k, , , drop = FALSE],
#                                                   curUse = currNeighborWC)

#     # Update locally missing water in c
#     missingWW <- missingWW - currNeighborWW
#     missingWC <- missingWC - currNeighborWC

#     if (allocationrule == "optimization") {
#       # Exit Neighbor Water Provision
#       if (any(missingWW < 0)) {
#         stop(paste0("More water than necessary provided to missingWW
#                      in toolNeighborWaterProvision in cell ", k,
#                      " by neighborcell ", c))
#       }
#       if (any(missingWC < 0)) {
#         stop(paste0("More water than necessary provided to missingWC
#                     in toolNeighborWaterProvision in cell ", k,
#                     " by neighborcell ", c))
#       }
#       if (all(missingWW <= 0) && all(missingWC <= 0)) {
#         break
#       }
#     } else {

#     }

#   } else {
#     stop("In toolRiverDischargeAllocation:
#           Please select `main` or `neighbor` for `iteration` argument
#           to indicate stage of discharge allocation.")
#   }

#   return(out)
# }
# # Case N.1: Upstream-Downstream Routing: missingWW and missingWC are outputs of function; routing runs completely through
#             # (1) all missingWater is reported to the respective closest neighbor cell
#             # (2) routing is repeated with neighbor cell as c 
#             #     (neighbor cells try to fulfill the missing water that has been reported to them)
#             # (3) water is allocated to the cell that have asked for it (proportional to their demand / by distance)
# # Case N.2: Optimization: Neighbor Irrigation is integrated into routing (try to fulfill on the go)
#             # (1) missingWater of c is reported to toolRiverDischargeAllocation
#             # (2) toolRiverDischargeAllocation is run again for n (Selbst-Aufruf innerhalb von toolRiverDischargeAllocation)
#             #     (loop through all neighboring cells of c and report back / update 
#             #      currNeighborWW, currNeighborWC, resNeighborWW, resNeighborWC)
#             #     Make sure to build in correct break/jumping out of function
#             # (3) Original river routing continues for next main cell
