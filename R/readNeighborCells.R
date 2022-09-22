#' @title readNeighborCells
#'
#' @description read file with neighbor cell and distance information
#'              derived from LPJmL grid
#'
#' @return empty magpie object with list data stored in attributes
#'
#' @author Felicitas Beier
#'
#' @importFrom magclass as.magpie
#'
#' @examples
#' \dontrun{
#' readSource("NeighborCells", convert = FALSE)
#' }
#'

readNeighborCells <- function() {

  # read in data
  neighborcellList <- readRDS("neighborCells.rds")

  # save in magpie object
  x               <- as.magpie(1)
  attr(x, "data") <- neighborcellList

  return(x)
}
