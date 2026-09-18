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
#' @export

toolSelectNeighborCell <- function(transDist,  rs = rs,
                                   neighborCells = neighborCells) {

  # empty list to assign neighbor cells in river structure list
  rs$neighborcell <- vector("list", length(rs$cells))
  rs$neighbordist <- vector("list", length(rs$cells))

  # append neighbor cells to river structure
  for (i in seq_along(rs$cells)) {

    if (!is.null(neighborCells[[i]])) {

      # Select neighbors to respective cell i.
      # Note that neighbor cells are sorted by distance in
      # the given object provided by readNeighborCells().
      neighbors <- neighborCells[[i]]

      # Exclude cells above chosen distance
      neighbors     <- neighbors[neighbors$dist < transDist, ]
      selectedCells <- neighbors$cellid

      # Exclude upstreamcells from neighbors
      if (!identical(rs$upstreamcells[[i]], numeric(0))) {
        selectedCells <- setdiff(selectedCells, rs$upstreamcells[[i]])
      }
      # Exclude downstreamcells from neighbors
      if (!identical(rs$downstreamcells[[i]], numeric(0))) {
        selectedCells <- setdiff(selectedCells, rs$downstreamcells[[i]])
      }

      # Assign selected neighbor cells and corresponding distances
      rs$neighborcell[[i]] <- selectedCells
      if (length(selectedCells) > 0) {
        rs$neighbordist[[i]] <- neighbors$dist[match(selectedCells, neighbors$cellid)]
      }
    }
  }

  return(rs)
}
