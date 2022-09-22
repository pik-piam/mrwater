#' @title       toolSelectNeighborCell
#' @description Selects cells in certain radius of current cell
#'
#' @param transDist Water transport distance allowed to fulfill locally
#'                  unfulfilled water demand
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'

toolSelectNeighborCell <- function(transDist) {

  # read in neighbor cells and transform to list
  neighborCells <- readSource("NeighborCells", convert = FALSE)
  neighborCells <- attr(neighborCells, "data")

  # read in river structure
  rs              <- readRDS(system.file("extdata/riverstructure_stn_coord.rds",
                                         package = "mrwater"))
  rs$neighborcell <- vector("list", length(rs$cells))

  # append neighbor cells to river structure
  for (i in seq_along(rs$cells)) {

    if (!is.null(neighborCells[[i]])) {

      # Sort by distance
      neighborCells[[i]] <- neighborCells[[i]][order(neighborCells[[i]]$dist), ]

      # Exclude cells above chosen distance
      rs$neighborcell[[i]] <- neighborCells[[i]]$cellid[neighborCells[[i]]$dist < transDist]

      # Exclude upstreamcells from neighbors
      if (identical(rs$upstreamcells[[i]], numeric(0))) {
        rs$neighborcell[[i]] <- setdiff(rs$neighborcell[[i]], rs$upstreamcells[[i]])
      }
      # Exclude downstreamcells from neighbors
      if (identical(rs$downstreamcells[[i]], numeric(0))) {
        rs$neighborcell[[i]] <- setdiff(rs$neighborcell[[i]], rs$downstreamcells[[i]])
      }
    }
  }

  return(rs)
}
