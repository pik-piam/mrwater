#' @title toolLPJarrayToMAgPIEmap
#' @description transforms object (array) with 67420 cells to be able to be used in plotmap function
#'
#' This tool function selects the required cells from object with
#' 67420 cells to be able to plot using plotmap or plotmap2 that
#' are applied to magpie object of size 59199 cells
#'
#' @param x magpie object to be transformed
#'
#' @return magpie object with correct dimension for use in plotmap
#' @author Felicitas Beier
#'
#' @import magclass
#'
#' @export


toolLPJarrayToMAgPIEmap <- function(x) {
  # select correct cells and transform to magpie object
  out <- as.magpie(x[magclassdata$cellbelongings$LPJ_input.Index],spatial=1)
  out <- toolCell2isoCell(out)

  return(out)
}
