#' @title toolLPJcellCoordinates
#' @description maps LPJ cells and coordinates
#'
#' @param x    magpie object for which coordinatese should be remapped
#' @param type direction of transformation (coord2lpj or lpj2coord)
#'
#' @return magpie object with coordinates in LPJ cell ordering
#' @author Felicitas Beier
#'
#' @import magclass
#' @importFrom madrat toolGetMapping
#'
#' @export


toolLPJcellCoordinates <- function(x, type) {

  # read in LPJmL mapping
  LPJcells <- toolGetMapping("LPJ_CellBelongingsToCountries.csv", type="cell", where="mappingfolder")
  # transform to coordinate style data with p separation
  LPJcells <- data.frame(LPJcells, data.frame("coordinates"=paste(gsub("\\.", "p", LPJcells$lon), gsub("\\.", "p", LPJcells$lat), sep="."),stringsAsFactors=F))
  # add cell number (LPJmL cell code)
  LPJcells <- data.frame(LPJcells, data.frame("cell"=paste(LPJcells$ISO,c(1:67420),sep="."),stringsAsFactors=F))
  # subset data frame
  LPJcells <- data.frame("cell"=LPJcells$cell,"coordinates"=LPJcells$coordinates,stringsAsFactors=F)


  if (type=="coord2lpj") {

    # sort cells
    out <- x[LPJcells$coordinates,,]
    # rename
    getCells(out) <- LPJcells$cell

  } else if (type=="lpj2coord") {

    # transform lpjcells to coordinates
    getCells(x) <- LPJcells$coordinates
    out <- x

  } else {
    stop("Please select direction of transformation via type argument: coord2lpj or lpj2coord")
  }

  return(out)
}
