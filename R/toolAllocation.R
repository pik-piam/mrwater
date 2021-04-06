#' @title       toolAllocation
#' @description This tool function executes the Allocation Algorithm in cell order of selected allocation rule
#'
#' @param y Current year of loop
#' @param rs River structure (list of river structure details and cell numbers including ordered cell number)
#' @param l_inout list of inputs that are at the same time outputs: required_wat_min_allocation Minimum (water requirement reserved per grid cell (as magpie object of correct dimension)); discharge (Discharge to be allocated (as magpie object of correct dimension)); frac_fullirrig (fraction of fullirrigation requirements that can be fulfilled)
#' @param l_in list of inputs: irrig_yieldgainpotential (yield gain potential through irrigation of proxy crops: magpie object with cellular and year dimension (as magpie object of correct dimension)); required_wat_fullirrig_ww (required withdrawal for full irrigation in specific cell (as magpie object of correct dimension)); required_wat_fullirrig_wc (required consumption for full irrigation in specific cell (as magpie object of correct dimension)); gainthreshold (Minimum yield gain in USD/ha (as scalar value))
#' @param allocationrule  Rule to be applied for river basin discharge allocation across cells of river basin ("optimization" (default), "upstreamfirst", "equality")
#' @param meancellrank cell ranking for different years (array). Note: only applicable when allocationrule "optimization" chosen
#'
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jan Philipp Dietrich
#'

toolAllocation <- function(y, rs, l_inout, l_in, allocationrule, meancellrank) {

  if (allocationrule=="optimization") {

    # cell number as numeric for surplus discharge allocation algorithm
    rs$cells <- as.numeric(gsub("(.*)(\\.)", "", rs$cells))

    for (o in (1:max(meancellrank[,y], na.rm=T))) { #test <- rs$cells[order(meancellrank[,y])]
      c <- rs$cells[meancellrank[,y]==o]

      l_inout <- toolAllocationAlgorithm(c=c, y=y, rs=rs, l_inout=l_inout, l_in=l_in)
    }

  } else if (allocationrule=="upstreamfirst") {
    for (o in 1:max(rs$calcorder)) {
      cells <- which(rs$calcorder==o)

      for (c in cells){
        l_inout <- toolAllocationAlgorithm(c=c, y=y, rs=rs, l_inout=l_inout, l_in=l_in)
      }
    }
  }

  out <- l_inout

  return(out)
}
