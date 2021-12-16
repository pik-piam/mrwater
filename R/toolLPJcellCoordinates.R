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
  lpjCells <- toolGetMapping("LPJ_CellBelongingsToCountries.csv",
                             type = "cell", where = "mcrommons")
  # transform to coordinate style data with p separation
  lpjCells <- data.frame(lpjCells, data.frame("coordinates" = paste(gsub("\\.", "p", lpjCells$lon),
                                                                    gsub("\\.", "p", lpjCells$lat), sep = "."),
                                              stringsAsFactors = FALSE))
  # add cell number (LPJmL cell code)
  lpjCells <- data.frame(lpjCells, data.frame("cell" = paste(lpjCells$ISO, c(1:67420), sep = "."),
                                              stringsAsFactors = FALSE))
  # subset data frame
  lpjCells <- data.frame("cell" = lpjCells$cell,
                         "coordinates" = lpjCells$coordinates,
                         stringsAsFactors = FALSE)


  if (type == "coord2lpj") {

    # sort cells
    out <- x[lpjCells$coordinates, , ]
    # rename
    getCells(out) <- lpjCells$cell

  } else if (type == "lpj2coord") {

    # transform lpjCells to coordinates
    getCells(x) <- lpjCells$coordinates
    out <- x

  } else {
    stop("Please select direction of transformation via type argument: coord2lpj or lpj2coord")
  }

  return(out)
}
