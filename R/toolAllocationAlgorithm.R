#' @title       toolAllocationAlgorithm
#' @description This tool function determines river surplus discharge allocation rules for calcRiverSurplusDischargeAllocation
#'
#' @param c Cell number
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
# toolAllocationAlgorithm <- function(c, irrig_yieldgainpotential) {
#
#   # River Structure
#   rs <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
#   # cells as numeric for surplus discharge allocation algorithm
#   rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))
#
#   # vector of downstreamcells of c
#   down <- unlist(rs$downstreamcells[[c]])
#   # vector of c in length of downstreamcells of c
#   lc   <- rep(c, length(rs$downstreamcells[[c]]))
#   # vector of 1s in length of downstreamcells of c
#   cc   <- rep(1:length(c), length(rs$downstreamcells[[c]]))
#
#   # Only cells where irrigation potential exceeds certain minimum threshold are (additionally) irrigated
#   irriggain <- (irrig_yieldgainpotential[c,y,,drop=F] > gainthreshold)
#
#   # available water for additional irrigation withdrawals
#   avl_wat_ww[c,y,][irriggain[,,,drop=F]] <- pmax(IO_discharge[c,y,,drop=F] - required_wat_min_allocation[c,y,,drop=F], 0)[irriggain[,,,drop=F]]
#
#   # withdrawal constraint
#   ww_constraint   <- (required_wat_fullirrig_ww[c,y,,drop=F]>0 & irriggain[,,,drop=F])
#
#   # how much withdrawals can be fulfilled by available water
#   frac_fullirrig[c,y,][ww_constraint[,,,drop=F]] <- pmin(avl_wat_ww[c,y,,drop=F][ww_constraint[,,,drop=F]] / required_wat_fullirrig_ww[c,y,,drop=F][ww_constraint[,,,drop=F]], 1)
#
#   if (length(down)>0) {
#     # consumption constraint
#     wc_constraint <- (required_wat_fullirrig_wc[c,y,,drop=F]>0 & ww_constraint[,,,drop=F])
#
#     # available water for additional irrigation consumption (considering downstream availability)
#     avl_wat_wc[c,y,][wc_constraint[,,,drop=F]]     <- pmax(apply((IO_discharge[down,y,,drop=F] - required_wat_min_allocation[down,y,,drop=F]), 3, min)[wc_constraint[,,,drop=F]], 0)
#     # how much consumption can be fulfilled by available water
#     frac_fullirrig[c,y,][wc_constraint[,,,drop=F]] <- pmin(avl_wat_wc[c,y,,drop=F][wc_constraint[,,,drop=F]] / required_wat_fullirrig_wc[c,y,,drop=F][wc_constraint[,,,drop=F]], frac_fullirrig[c,y,,drop=F][wc_constraint[,,,drop=F]])
#   }
#
#   # adjust discharge in current cell and downstream cells (subtract irrigation water consumption)
#   IO_discharge[c(down,c),y,][ww_constraint[c(cc,1),,,drop=F]] <- (IO_discharge[c(down,c),y,,drop=F] - required_wat_fullirrig_wc[c(lc,c),y,,drop=F] * frac_fullirrig[c(lc,c),y,,drop=F])[ww_constraint[c(cc,1),,,drop=F]]
#   # update minimum water required in cell:
#   required_wat_min_allocation[c,y,][ww_constraint[,,,drop=F]] <- (required_wat_min_allocation[c,y,,drop=F] + frac_fullirrig[c,y,,drop=F] * required_wat_fullirrig_ww[c,y,,drop=F])[ww_constraint[,,,drop=F]]
#
#   return(list(
#     x=out,
#     weight=NULL,
#     unit="mio. m^3",
#     description=description,
#     isocountries=FALSE))
# }
