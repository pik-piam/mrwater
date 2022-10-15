#' @title       toolSelectNeighborCell
#' @description Selects cells in certain radius of current cell
#'
#' @param transDist     Water transport distance allowed to fulfill locally
#'                      unfulfilled water demand
#' @param rs            River structure list
#' @param neighborCells List of neighboring cells for all river cells
#'
#' @return magpie object in cellular resolution
#' @author Felicitas Beier, Jens Heinke
#'

toolSelectNeighborCell <- function(transDist,  rs = rs,
                                   neighborCells = neighborCells) {

  # empty list to assign neighbor cells in river strucutre list
  rs$neighborcell <- vector("list", length(rs$cells))
  rs$neighbordist <- vector("list", length(rs$cells))

  # append neighbor cells to river structure
  for (i in seq_along(rs$cells)) {

    if (!is.null(neighborCells[[i]])) {

      # Sort by distance
      neighborCells[[i]]   <- neighborCells[[i]][order(neighborCells[[i]]$dist), ]

      # Exclude cells above chosen distance
      rs$neighborcell[[i]] <- neighborCells[[i]]$cellid[neighborCells[[i]]$dist < transDist]

      # Exclude upstreamcells from neighbors
      if (!identical(rs$upstreamcells[[i]], numeric(0))) {
        rs$neighborcell[[i]] <- setdiff(rs$neighborcell[[i]], rs$upstreamcells[[i]])
      }
      # Exclude downstreamcells from neighbors
      if (!identical(rs$downstreamcells[[i]], numeric(0))) {
        rs$neighborcell[[i]] <- setdiff(rs$neighborcell[[i]], rs$downstreamcells[[i]])
      }

      # Assign distance to selected neighborcells
      for (k in rs$neighborcell[[i]]) { #@Jan: Can this be done without a loop?
        tmp <- neighborCells[[i]]$dist[neighborCells[[i]]$cellid == k]
        rs$neighbordist[[i]] <- c(rs$neighbordist[[i]], tmp)
      }
    }
  }

  return(rs)
}
