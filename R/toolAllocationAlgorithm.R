#' @title       toolAllocationAlgorithm
#' @description This tool function determines river surplus discharge allocation rules for calcRiverSurplusDischargeAllocation
#'
#' @param irrig_yieldgainpotential Yield gain potential through irrigation of proxy crops: magpie object with cellular and year dimenstion
#' @param gainthreshold Minimum yield gain: scalar value
#'
#'
#' @importFrom madrat calcOutput
#' @importFrom magclass collapseNames getNames as.magpie getCells setCells mbind setYears dimSums
#' @importFrom stringr str_split
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'
#
# toolAllocationAlgorithm <- function(irrig_yieldgainpotential) {
#
#   # Initialization of temporary and output variables
#   O_discharge    <- as.array(discharge[,y,paste(EFP,scen,sep=".")])[,1,1]
#   frac_fullirrig <- array(data=0,dim=67420)
#   avl_wat_ww     <- array(data=0,dim=67420)
#   avl_wat_wc     <- array(data=0,dim=67420)
#   O_required_wat_min_allocation <- as.array(required_wat_min_allocation[,y,paste(EFP,scen,sep=".")])[,1,1]
#
#   # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
#   if (irrig_yieldgainpotential[c,y] > gainthreshold) {
#     # available water for additional irrigation withdrawals
#     avl_wat_ww <- max(O_discharge[c]-O_required_wat_min_allocation[c],0)
#
#     # withdrawal constraint
#     if (required_wat_fullirrig_ww[c,y]>0) {
#       # how much withdrawals can be fulfilled by available water
#       frac_fullirrig[c] <- min(avl_wat_ww/required_wat_fullirrig_ww[c,y],1)
#
#       # consumption constraint
#       if (required_wat_fullirrig_wc[c,y]>0 & length(rs$downstreamcells[[c]])>0) {
#         # available water for additional irrigation consumption (considering downstream availability)
#         avl_wat_wc          <- max(min(O_discharge[rs$downstreamcells[[c]]] - O_required_wat_min_allocation[rs$downstreamcells[[c]]]),0)
#         # how much consumption can be fulfilled by available water
#         frac_fullirrig[c]   <- min(avl_wat_wc/required_wat_fullirrig_wc[c,y],frac_fullirrig[c])
#       }
#
#       # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#       O_discharge[c(rs$downstreamcells[[c]],c)] <- O_discharge[c(rs$downstreamcells[[c]],c)] - required_wat_fullirrig_wc[c,y]*frac_fullirrig[c]
#       # update minimum water required in cell:
#       O_required_wat_min_allocation[c] <- O_required_wat_min_allocation[c] + frac_fullirrig[c]*required_wat_fullirrig_ww[c,y]
#     }
#   }
#
#   return(list(
#     x=out,
#     weight=NULL,
#     unit="mio. m^3",
#     description=description,
#     isocountries=FALSE))
# }
