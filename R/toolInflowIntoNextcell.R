#' @title toolInflowIntoNextcell
#' @description calculates inflow into nextcell for several cells with same
#' calcorder accounting for fact that one cell can discharge into same nextcell
#'
#' @param cell current cell or cells that are calculated in this step of the loop
#' @param previous_inflow inflow into nextcell
#' @param cell_discharge discharge in current cell
#'
#' @return magpie object
#' @author Felicitas Beier

toolInflowIntoNextcell <- function(cell, previous_inflow, cell_discharge) {

  # river structure
  rs  <- readRDS(system.file("extdata/riverstructure_stn_coord.rds", package="mrwater"))
  out <- previous_inflow

  # inflow into nextcell
  if (any(rs$nextcell[cell] > 0)) {
    current  <- cell[rs$nextcell[cell] > 0]
    nextcell <- rs$nextcell[current]

    # inflow needs to be calculated for all nextcells separately to account for
    # fact that there can be several cells that discharge into same nextcell
    while (length(current) > 0) {
      dn            <- duplicated(nextcell)
      rest_c        <- current[dn]
      current       <- current[!dn]
      rest_nextcell <- nextcell[dn]
      nextcell      <- nextcell[!dn]

      out[nextcell,,] <- previous_inflow[nextcell,,] + cell_discharge[current,,]

      nextcell <- rest_nextcell
      current  <- rest_c
    }
  }

  return(out)
}
